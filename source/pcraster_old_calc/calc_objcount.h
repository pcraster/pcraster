#ifndef OBJCOUNT_INCLUDED
#define OBJCOUNT_INCLUDED

namespace calc {

//! from more effective C++ item 27
template<class BeingObjCount>class ObjCount {
public:
   // class TooManyObjects{};   // for throwing exceptions

   static int objectCount() { return numObjects; }

protected:
   ObjCount();
   ObjCount(const ObjCount& rhs);

   ~ObjCount() {
    DEVELOP_PRECOND(numObjects>0);
    --numObjects;
   }

private:
   static int numObjects;
// static const size_t maxObjects;

   void init();
};

template<class BeingObjCount>
  ObjCount<BeingObjCount>::ObjCount()
{ init(); }

template<class BeingObjCount>
  ObjCount<BeingObjCount>::ObjCount(const ObjCount<BeingObjCount>&)
{ init(); }

template<class BeingObjCount>
  void ObjCount<BeingObjCount>::init()
{
//   if (numObjects >= maxObjects) throw TooManyObjects();
     ++numObjects;
}

template<class BeingObjCount>class ObjCounter {
  const char *d_className;
public:
  ObjCounter(const char *name):d_className(name) {};
  ~ObjCounter() {
    if(ObjCount<BeingObjCount>::objectCount() != 0) {
#ifdef DEBUG_DEVELOP
      fprintf(stderr,"Class %s %u \n",d_className,
       ObjCount<BeingObjCount>::objectCount());
      POSTCOND(ObjCount<BeingObjCount>::objectCount() == 0);
#endif
    }
  };
};

}

#endif
