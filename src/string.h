// Cplus project, string class optimized for pipelined parsers.
// copyright David Krauss, created 10/19/11

#ifndef CPLUS_STRING
#define CPLUS_STRING

#include <algorithm>
#include <random>
#include <cassert>

namespace cplus {

class string_pool {
public:
	struct client {
		string_pool *a;
		client *prec, *succ; // would be private but derived provides pool dump facility
		
		bool is_registered() const // only a moved-from value may be unregistered
			{ return prec != nullptr; }
		bool is_writer() const // only one object within the pool may be open for writing
			{ return a->writer == this; }
		
		client( string_pool &in_a ) // construct new object in particular allocator
			: a( & in_a )
			{ register_(); }
		client( client const &o ) // nothing special, construct object to receive a copy of another
			: a( o.a )
			{ register_(); }
		client( client &&o ) // move construction invalidates source and slightly reduces overhead
			: a( o.a ) {
			if ( o.prec == nullptr ) {
				register_();
				return;
			}
			/*if ( o.prec == nullptr ) {
				register_();
				#if DUMP_STRING_ACTIVITY
				std::cerr << a->name << ": resurrect " << &o << " as " << this << "\n";
				#endif
				return;
			}*/
			inherit_links( o );
			o.prec->succ = o.succ->prec = this; // usurp source's links
			prec = o.prec;
			succ = o.succ;
			//std::cerr << a->handle_count << " + " << this << " - " << & o << " ( " << a->name << ": " << prec << " -- " << succ << " )\n";
			o.invalidate(); // invalidate source
			assert ( ( a->handle_count == 1 ) == ( succ == this && succ == prec && succ == a->handle_first ) );
		}
		
		client &operator= ( client const &o ) {
			assert ( & o != this ); // Derived class must handle copy to self.
			assert ( & o != a->writer ); // Derived class must close source writer before copy within same pool.
			if ( o.a == a && prec != nullptr ) {
				//if ( a->writer == &o ) a->writer = static_cast< string_base * >( this );
				return * this;
			}
			unregister();
			a = o.a;
			//if ( a->writer == &o ) a->writer = static_cast< string_base * >( this );
			register_();
			return * this;
		}
		client &operator= ( client &&o ) {
			assert ( & o != this ); // Would just be early exit, but this shouldn't happen.
			assert ( a == o.a ); // Derived class must do a copy, can't move between pools.
			if ( prec == nullptr ) register_();
			inherit_links( o );
			o.unregister();
			o.invalidate();
			return * this;
		}
		~client() noexcept { unregister(); }
	
	private:
		void register_() {
			//static std::minstd_rand r;
			
			prec = succ = this;
			client *succ_val = a->handle_first; // common case: add to end of list (circular so before beginning = end)
			++ a->handle_count;
			//if ( r() % ++ a->handle_count == 0 ) { // first and O(1/N) case: add to beginning of list
				if ( a->handle_count == 1 ) {
					//std::cerr << "initialize list\n";
					succ_val = this; // or (re)initialize list
			//	} //else std::cerr << "add to front\n";
				a->handle_first = static_cast< string_base * >( this ); // new string is new beginning
			}
			std::swap( prec, succ_val->prec );
			std::swap( succ, prec->succ );
			
			//std::cerr << a->handle_count << " + " << this << " ( " << a->name << ": " << prec << " -- " << succ << " )\n";
			assert ( ( a->handle_count == 1 ) == ( succ == this && succ == prec && succ == a->handle_first ) );
		}
		void unregister() {
			if ( ! prec ) return;
			prec->succ = succ;
			succ->prec = prec;
			if ( a->handle_first == this ) a->handle_first = static_cast< string_base * >( succ );
			if ( a->writer == this ) a->writer = nullptr;
			-- a->handle_count;
			
			//std::cerr << a->handle_count << " - " << this << " ( " << a->name << ": " << prec << " -- " << succ << " )\n";
			assert ( ( a->handle_count <= 1 ) == ( succ == prec && succ == a->handle_first ) );
		}
		
		void invalidate() {
			assert ( a->handle_first != this );
			assert ( a->writer != this );
			prec = nullptr;
		}
		void inherit_links( client const &o ) { // this is the only way writership may be passed without writing something
			if ( a->handle_first == & o ) a->handle_first = static_cast< string_base * >( this );
			if ( a->writer == this ) {
				a->writer = nullptr;
			} else if ( a->writer == & o ) {
				a->writer = this;
				#if DUMP_STRING_ACTIVITY
				std::cerr << a->name << ": move writer = " << a->writer << '\n';
				#endif
			}
		}
	};
	struct string_base : client {
		char *p;
		
		void close() {
			* a->bump() = 0; // write zero termination
			std::size_t length = a->pen - p - 1;
			if ( length < std::numeric_limits< std::uint8_t >::max() ) {
				p[ -1 ] = length; // write length prefix
			} else {
				std::size_t length_space = sizeof (std::size_t);
				void *length_p = p - 1;
				util::align( alignof (std::size_t), 0, length_p, length_space );
				// size_space is decreased by bytes needed for alignment, but we want to increase allocation
				auto old_end = a->bump( sizeof (std::size_t) * 2 - length_space );
				
				p = std::copy_backward( p, old_end, a->pen );
				p[ -1 ] = std::numeric_limits< std::uint8_t >::max(); // write overflow indicator
				* reinterpret_cast< std::size_t * >( p - 1 - sizeof (std::size_t) ) = length; // and actual size
			}
			
			#if DUMP_STRING_ACTIVITY
			std::cerr << "finalized prefix @ " << (void*) ( p - 1 ) << ", string to " << (void*) a->pen << '\n';
			/*if ( writer_s->p != pen - 1 && pen[ -2 ] == char( 0xff ) ) {
				std::cerr << "uninitialized\n";
			}*/
			std::cerr << a->name << ": preempt writer " << (void *) this << " = " << (void *) p << ": " << p << '\n';
			#endif
			
			a->writer = nullptr;
		}
		
		string_base( string_pool &in_a )
			: client( in_a ), p( nullptr ) { a->allocate( * this, 0 ); }
	};
	
private:
	string_base *handle_first;
	std::size_t handle_count;
	
	enum { chunk_size = 1 << 20 };
	typedef std::unique_ptr< char[] > chunk_handle;
	std::vector< chunk_handle > chunks;
	char *cur_chunk;
	
	void add_chunk() {
		chunk_handle c( cur_chunk = new char[ chunk_size ] );
		auto chunk_insertion = std::lower_bound( chunks.begin(), chunks.end(), c );
		chunks.insert( chunk_insertion, std::move( c ) );
	}
public:
	char *pen;
	client *writer;
	
	char const *name;
	
	string_pool( char const *in_name )
		: handle_first( nullptr ), handle_count( 0 ), writer( nullptr ), name( in_name )
		{ add_chunk(); pen = cur_chunk; }
	string_pool( string_pool const & ) = delete;
/*		: handle_first( nullptr ), handle_count( 0 ), writer( nullptr ), name( "clone" )
		{ add_chunk(); pen = * cur_chunk; }*/
	string_pool( string_pool && ) = default;
	~string_pool() noexcept { assert ( handle_count == 0 ); }

	char *bump( std::ptrdiff_t n = 1, bool do_dump = true ) {
		if ( n > cur_chunk + chunk_size - pen ) return bump_chunk( n, do_dump );
		#if DUMP_STRING_ACTIVITY
		if ( n > 1 ) std::cerr << "internal bump " << n << '\n';
		#endif
		pen += n;
		//std::uninitialized_fill_n( pen - n, n, 0xff );
		return pen - n;
	}
	
	char *bump_chunk( std::ptrdiff_t n = 1, bool do_dump = true ) {
		if ( n > chunk_size ) too_big: throw std::length_error( "String size exceeds pool chunk size." );
		
		std::vector< bool > used_chunks( chunks.size() );
		int unused_chunk_count = chunks.size();
		std::size_t handles_seen = 0; 
		
		auto handle = handle_first;
		if ( handle_count ) do {
			// Finds least chunk end not less than handle.
			// Comparing pointers between arrays like this is illegal; may break under memory segmentation.
			
			if ( handle->p ) {
				auto c = std::upper_bound( chunks.begin(), chunks.end(), handle->p,
					[]( char *lhs, chunk_handle const &rhs ) { return lhs < rhs.get() + chunk_size; } );
				auto bit = used_chunks[ c - chunks.begin() ];
				if ( ! bit ) -- unused_chunk_count;
				bit = true;
			}
			handle = static_cast< string_base * >( handle->succ );
			++ handles_seen;
		} while ( unused_chunk_count && handle != handle_first );
		
		if ( unused_chunk_count ) {
			#if DUMP_STRING_ACTIVITY || DUMP_CHUNK_ACTIVITY
			std::cerr << name << ": reuse " << std::find( used_chunks.begin(), used_chunks.end(), false ) - used_chunks.begin() << '\n';
			#endif
			cur_chunk = chunks[ std::find( used_chunks.begin(), used_chunks.end(), false ) - used_chunks.begin() ].get();
		} else {
			#if DUMP_STRING_ACTIVITY || DUMP_CHUNK_ACTIVITY
			std::cerr << name << ": add chunk " << chunks.size()
				<< ", after traversing " << handles_seen << " handles of " << handle_count << "\n";
			if ( do_dump ) dump();
			#endif
			add_chunk();
		}
		
		if ( writer ) {
			string_base *writer_s = static_cast< string_base * >( writer );
			if ( pen - writer_s->p + n + 1 > chunk_size ) goto too_big;
			
			#if DUMP_STRING_ACTIVITY || DUMP_CHUNK_ACTIVITY
			std::cerr << "carry over from bump " << n << " @ " << (void*) cur_chunk << '\n';
			std::cerr.write( writer_s->p, pen - writer_s->p );
			#endif
			pen = std::copy( writer_s->p, pen, cur_chunk + 1 ) + n;
			writer_s->p = cur_chunk + 1;
		
		} else pen = cur_chunk + n;
		
		//std::uninitialized_fill_n( pen - n, n, 0xff );
		
		return pen - n;
	}
	
	void dump();
	
	void allocate( string_base &client, std::size_t n ) {
		if ( writer ) { // Close writer. This should have some kind of polymorphism.
			static_cast< string_base * >( writer )->close();
		}
		
		client.p = bump( n + 1, false ) + 1;
		writer = & client;
		
		#if DUMP_STRING_ACTIVITY
		std::cerr << name << ": open writer = " << writer << ": " << (void*) client.p << '\n';
		#endif
	}
	
	typedef char value_type;
};
string_pool literal_string_alloc( "literal" );

class string : public string_pool::string_base {
public:
	explicit string( string_pool &a = literal_string_alloc ) : string_base( a ) {}
	
	string( string const &o )
		: string_base( o ) {
		if ( a->writer == & o ) {
			std::size_t n = o.size();
			a->allocate( * this, n );
			std::char_traits< char >::copy( p, o.p, n );
		#if DUMP_STRING_ACTIVITY
			std::cerr << a->name << ": copy ";
		} else std::cerr << a->name << ": clone ";
		
		std::cerr << & o << ": " << (void*) o.p;/* << " size " <<*/ size(); std::cerr << " to " << this << ": " << (void*) p << '\n';
		#else
		}
		#endif
	}
	string( string &&o )
		: string_base( std::move( o ) ) {
		#if DUMP_STRING_ACTIVITY
		std::cerr << a->name << ": move " << static_cast< string & >( * this ) << " @ " << (void*) p << '\n';
		#endif
	}
	
	string( char const *s, string_pool &in_a = literal_string_alloc )
		: string_base( in_a ) {
		#if DUMP_STRING_ACTIVITY
		std::cerr << a->name << ": open " << s << '\n';
		#endif
		auto n = std::char_traits< char >::length( s );
		std::char_traits< char >::copy( a->bump( n ), s, n );
	}
	string( string const &o, string_pool &in_a )
		: string_base( in_a ) {
		if ( a == o.a ) {
			* this = std::move( o );
		} else {
			std::size_t n = o.size();
			std::char_traits< char >::copy( a->bump( n ), o.p, n );
		}
	}
	~string() noexcept
		{ kill_content(); }
	
	operator std::string() const { return { begin(), end() }; }
	
	string &operator= ( string &&o ) {
		kill_content();
		if ( a != o.a ) {
			if ( o.a->writer == & o ) {
				o.a->writer = this; // inherit writability in new pool
			}
			string_base::operator= ( o );
		} else {
			string_base::operator= ( std::move( o ) ); // always shallow, inherit writability
		}
		return * this;
	}
	string &operator= ( string const &o ) {
		kill_content(); // erase destination writer
		if ( o.a->writer == & o ) {
			o.a->allocate( * this, 0 ); // terminate source writer (bogus allocation, maybe wrong pool)
			o.a->writer = nullptr;
		}
		string_base::operator= ( o );
		return * this;
	}
	
	void open() {
		if ( a->writer == this ) return;
		auto old_begin = begin(), old_end = end();
		a->allocate( * this, size() );
		std::copy( old_begin, old_end, p );
	}
	void kill_content() noexcept {
		if ( a->writer == this ) {
			#if DUMP_STRING_ACTIVITY
			std::cerr << "dead " << static_cast< string & >( * this ) << '\n';
			#endif
			a->bump( - ( size() + 1 ) );
			a->writer = nullptr;
		}
	}

	string &assign( char *first, char *last ) {
		a->allocate( * this, last - first );
		std::copy( first, last, p );
		return * this;
	}
	
	string &operator= ( char const *rhs )
		{ return * this = { rhs, * a }; }

	string &operator= ( char rhs ) {
		if ( a->writer == this ) {
			a->pen = p;
			* a->bump() = rhs;
		} else {
			a->allocate( * this, 1 );
			* p = rhs;
		}
		return * this;
	}
	friend string &operator+= ( string &lhs, char rhs ) {
		assert ( lhs.a->writer == & lhs );
		char *b = lhs.a->bump();
		* b = rhs;
		return lhs;
	}
	friend string &operator+= ( string &lhs, char const *rhs ) {
		assert ( lhs.a->writer == & lhs );
		auto old_length = lhs.size();
		auto n = std::char_traits< char >::length( rhs );
		std::char_traits< char >::copy( lhs.a->bump( n ), rhs, n );
		assert ( lhs.size() == old_length + n );
		return lhs;
	}
	
	std::size_t size() const {
	/*	{ return std::char_traits< char >::length( p ); }
	std::size_t size()*/
		#if DUMP_STRING_ACTIVITY
		if ( this != a->writer && a->writer && p == static_cast< string * >( a->writer )->p ) {
			std::cerr << "bad clone\n";
		}
		#endif
		std::size_t ret;
		if ( this == a->writer ) {
			ret = a->pen - p;
		} else {
			ret = reinterpret_cast< std::uint8_t * >( p )[ -1 ];
			if ( ret == std::numeric_limits< std::uint8_t >::max() ) {
				ret = reinterpret_cast< std::size_t * >( p - 1 )[ -1 ];
			}
		}
		#if DUMP_STRING_ACTIVITY
		std::cerr << "size (" << std::string( p, 8 ) << ") = " << ret << '\n';
		if ( p + ret > a->pen ) {
			std::cerr << "bad size\n";
		}
		if ( ret != 0 && * p == 0 ) {
			if ( this == a->writer ) std::cerr << "in writer\n";
			std::cerr << a->name << ": size = " << ret << " @ " << this << ": " << (void*) p << ", why not empty?\n";
		}
		#endif
		return ret;
	}
	void resize( std::size_t n ) {
		#if DUMP_STRING_ACTIVITY
		if ( a->writer != this ) {
			std::cerr << a->name << ": bad resize\n";
		}
		#endif
		a->pen = p + n;
	} // can only shrink a string open for modification
	
	char const *c_str() const {
		if ( this == a->writer ) {
			* a->bump() = 0;
			a->bump( -1 );
		}
		return p;
	}
	char *begin() { return p; }
	char *end() { return p + size(); }
	char const *begin() const { return p; }
	char const *data() const { return p; }
	char const *end() const { return p + size(); }
	
	char &front() { return * begin(); }
	char &back() { return end()[ -1 ]; } // requires being open for modification
	char &operator[] ( std::ptrdiff_t n ) { return begin()[ n ]; }
	
	std::size_t find( char c ) const // requires that the character is actually found!
		{ for ( std::size_t n = 0; ; ++ n ) if ( p[ n ] == c ) return n; }
	
	/*friend bool operator== ( string &lhs, string &rhs ) {
		auto size = lhs.size();
		if ( size != rhs.size() ) return false;
		return std::char_traits< char >::compare( lhs.p, rhs.p, size ) == 0;
	}
	friend bool operator== ( string &lhs, string const &rhs ) {
		auto size = lhs.size();
		if ( size != rhs.size() ) return false;
		return std::char_traits< char >::compare( lhs.p, rhs.p, size ) == 0;
	}*/
	friend bool operator== ( string const &lhs, string const &rhs ) {
		#if DUMP_STRING_ACTIVITY
		std::cerr << lhs << " (" << lhs.a->name << ") compare == " << rhs << " (" << rhs.a->name << ")" << '\n';
		#endif
		auto size = lhs.size();
		if ( size != rhs.size() ) return false;
		return std::char_traits< char >::compare( lhs.p, rhs.p, size ) == 0;
	}
	/*friend bool operator!= ( string &lhs, string &rhs ) { return ! ( lhs == rhs ); }
	friend bool operator!= ( string &lhs, string const &rhs ) { return ! ( lhs == rhs ); }*/
	friend bool operator!= ( string const &lhs, string const &rhs ) { return ! ( lhs == rhs ); }
	friend bool operator< ( string const &lhs, string const &rhs ) {
		#if DUMP_STRING_ACTIVITY
		std::cerr << lhs << " (" << lhs.a->name << ") compare < " << rhs << " (" << rhs.a->name << ")" << '\n';
		#endif
		auto l_size = lhs.size(), r_size = rhs.size();
		auto cmp = std::char_traits< char >::compare( lhs.p, rhs.p, std::min( l_size, r_size ) );
		return cmp < 0 || ( cmp == 0 && l_size < r_size );
	}
	
	bool empty() const /*{ return * p == 0; }
	bool empty() */{ return this == a->writer? p == a->pen : * p == 0; }
	void clear() { // re-opens the string for modification, inefficient for only clearing
		kill_content();
		* this = string( * a ); // move assignment operator
	}
	
	friend void swap( string &lhs, string &rhs ) {
		assert ( lhs.a == rhs.a );
		std::swap( lhs.p, rhs.p );
	}
	
	void pop_back() { -- a->pen; }
	
	friend std::ostream &operator<< ( std::ostream &s, string const &o )
		{ s.rdbuf()->sputn( o.data(), o.size() ); return s; }
	
	void dump_siblings() {
		string *pen = this;
		do {
			if ( pen != a->writer ) {
				if ( * pen->data() == '\n' ) std::cerr << "(" << pen->size() << " newlines) ";
				else std::cerr << * pen << " ";
			}
			pen = static_cast< string * >( pen->succ );
		} while ( pen != this );
		std::cerr << "\n\n\n";
	}
};

void string_pool::dump() {
	std::cerr << name << " pool contents:\n";
	static_cast< string * >( handle_first )->dump_siblings();
}

int stoi( string const &s ) { return stoi( std::string( s ) ); }

}
namespace std {
template<>
struct hash< cplus::string > {
	std::size_t operator() ( cplus::string const &s ) const {
		std::size_t ret = static_cast< std::size_t >( 0x597316AC597316AC );
		for ( auto c : s ) ret *= c; // no null bytes allowed!
		return ret;
	}
};
}

#endif
