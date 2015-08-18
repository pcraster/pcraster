#ifndef INCLUDED_COM_AUTOARRAYPTR
#define INCLUDED_COM_AUTOARRAYPTR

namespace com {

// copied verbatim from Effective C++ CD
// listing 3  Reeves article
// (c) Reeves
template<class X>
  class auto_array_ptr {
    X* p_;
  public:
    auto_array_ptr(X* p = 0) throw() : p_(p) {}
    auto_array_ptr(auto_array_ptr<X>& ap) throw() :
      p_(ap.release()) {}
    ~auto_array_ptr() {delete[ ]p_;}
    void operator=(auto_array_ptr<X>& rhs);

    X& operator*() throw() {return *p_;}
    X& operator[ ](int i) throw() {return p_[i];}
    X operator[ ](int i) const throw() {return p_[i];}
    X* get() const throw() {return p_;}
    //! return a modifiable ptr (added CW)
    X* ptr()       throw() {return p_;}
    X* release() throw() {return reset(0);}
    X* reset(X* p) throw() {X* tp = p_; p_ = p; return tp;}

    static void remove(X*& x) {X* tp=x; x=0; delete[ ]tp;}
  };

}

#endif
