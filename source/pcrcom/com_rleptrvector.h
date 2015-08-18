#ifndef INCLUDED_COM_RLEPTRVECTOR
#define INCLUDED_COM_RLEPTRVECTOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // RLEPtrVector declarations.
}



namespace com {

template<typename T,size_t TSize >
class RLEItem {
 public:
  // only specilization allowed
  // HMM bcc wil ze hier ook hebben 
  static T* basePtr(void *rlePtr) {
      return (T *)(((int)rlePtr) & 0xFFFFFFFC);
  }
  static size_t count(void *rlePtr) {
      return (((int)rlePtr) & 0x3);
  }
  static size_t maxCount() {
      return 0x3;
  }
};

template<typename T>
class RLEItem<T,4> {
  public:
    static T* basePtr(void *rlePtr) {
      return (T *)(((int)rlePtr) & 0xFFFFFFFC);
    }
    static size_t count(void *rlePtr) {
      return (((int)rlePtr) & 0x3);
    }
    static size_t maxCount() {
      return 0x3;
    }
};

template<typename T>
class RLEItem<T,12> : public RLEItem<T,4> {
};

typedef union ElemNode {
    //! containing basePtr and count
    char   *rlePtr;
    //! an extended count, previous d_data entry has highest count
    /*!
     The highest value possible in count marks that the next entry 
     in d_data holds the count
     */
    size_t extendedCount;
} ElemNode;


//! Run Length Encoded Pointer Vector
/*!
  Pointers to items that are adjacent in memory can be stored effienctly under
  the assumption that the items have a certain allignment. For example 8 byte
  data items alligned on 8 bytes, always have the last 3 bits set to 0. These
  <i>always 0 bits</i> are used to encode a runlength.

  The pointer (ptr) stored in the vector thus have 2 fields: basePtr and count.
  The size in bits of count is 2-log of the anticipated allignment.

  BasePtr and count together form a begin and past-the-end in iterator sense,
  thus a single ptr, or range of ptr of size is encoded as the ptr -address
  with count 1. The count 0 is the marker that the next d_data element is an
  extendedCount larget then maxCount.

  Note that the container can not hold 0 ptrs.

  Note that the class should be able to work unmodified on 64 bits and the win32 /LARGEADDRESSAWARE (default in pcrtree) since it only uses the last alignment bits, not the first (sign) bit that makes /LARGEADDRESSAWARE to fail.
*/
template<typename T>
class RLEPtrVector
{

public:
  typedef char                        BYTE;
  typedef RLEItem<T,sizeof(T)>  ITEM;

  enum  { itemSize = sizeof(T) };




  //! Assignment operator. NOT IMPLEMENTED.
  RLEPtrVector&           operator=           (const RLEPtrVector& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RLEPtrVector               (const RLEPtrVector& rhs);

  size_t                d_nr;

  /*! \brief the data
   * both front and back are ALWAYS of type rlePtr;
   */
  typedef std::vector<ElemNode> Data;
  Data                  d_data;
  //! ptr to the last two elements
  ElemNode             *d_back;


  T*                 itemBasePtr(size_t pos) const {
    return ITEM::basePtr(d_data[pos].rlePtr);
  }
  size_t                shortRunLength(size_t pos) const {
    return ITEM::count(d_data[pos].rlePtr);
  }
  size_t                totalRunLength(size_t pos) const {
    int c = shortRunLength(pos);
    if (!c) {
      DEVELOP_PRECOND(d_data.size() > pos+1);
      return d_data[pos+1].extendedCount;
    }
    return c;
  }

public:
  class const_iterator:
   public std::iterator<std::forward_iterator_tag, T *> {
     const RLEPtrVector *d_c;
     size_t d_basePos;
     size_t d_rlPos;
     size_t d_rlSize; // as past the end operator
     void newBasePos() {
         d_rlSize = d_c->totalRunLength(d_basePos);
         // no single runlenght can hold more than the whole container
         DEVELOP_PRECOND(d_rlSize <= d_c->size());
         d_rlPos  = 0;
     }
    /* void dump () {
     *  std::cout << "basePos(" << d_basePos << ") rlPos(" << d_rlPos
     *          << ") rlSize(" << d_rlSize << ")\n"; }
     */
    public:
     const_iterator(const RLEPtrVector& c):
         d_c(&c), d_basePos(0) {
            newBasePos();
         };
     //! only the end iterator
     const_iterator(const RLEPtrVector& c, int /* bogusToMark_end() */):
         d_c(&c), d_basePos(c.d_back-&(c.d_data[0]))
         {
           DEVELOP_PRECOND(d_basePos < c.size() ||
                           (d_basePos==0 && c.empty()));
           newBasePos();
           d_rlPos=d_rlSize; // past-the-end
         };
     bool operator!=(const const_iterator& rhs) {
       return !(*this == rhs);
     }
     /*!
      * \todo must comparing iters from 2 distinct
      *       containers be defined?
      */
     bool operator==(const const_iterator& rhs) {
       return // d_c==rhs.d_c
              d_basePos==rhs.d_basePos && d_rlPos == rhs.d_rlPos;
     }
     void operator++() {
       d_rlPos++;
       if (d_rlPos == d_rlSize) {
         // do not advance beyond last one
         if (d_c->d_back != &(d_c->d_data[d_basePos])) {
          if (d_rlPos > ITEM::maxCount())
            d_basePos++; // advance over extended count
          d_basePos++;
          newBasePos();
         }
       }
       // std::cout << "\n++ "; dump();
     }
     T * operator*() {
       // std::cout << "\n* "; dump();
       return d_c->itemBasePtr(d_basePos)+d_rlPos;
     }
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RLEPtrVector               ():
                     d_nr(0) {
                        DEVELOP_PRECOND(sizeof(typename RLEPtrVector::BYTE)==1);
                        ElemNode e;
                        e.extendedCount = 0;
                        d_data.push_back(e); // item as 0-ptr special case
                        d_data.push_back(e); // with 0 extendCount
                        d_back = &(d_data[0]);
                     }

  /* virtual */    ~RLEPtrVector              ()
                        {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  inline void push_back(const T *elem);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  inline size_t size()    const;
  inline bool   empty()   const;


  const_iterator begin()  const {
    return const_iterator(*this);
  }
  const_iterator end()    const {
    return const_iterator(*this,-1);
  }

  inline size_t capacity() const;

  //! debug
  inline void   dump() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------
/*!
 * the element is copied into the vector
 * \todo test inserting duplicates
 * \pre elem is not 0
 */
template<typename T>
void RLEPtrVector<T>::push_back(const T *elem) {
  DEVELOP_PRECOND(elem);
  d_nr++;
  T    *backItem=ITEM::basePtr(d_back[0].rlePtr);
  int    shortCount=ITEM::count(d_back[0].rlePtr);
  // optimized for the case where distance is > 0
  int   distance=elem-backItem;
  if (distance == shortCount){
    // add to "shortCount"
    if (shortCount == (int)ITEM::maxCount()) {
      // short count exhausted
      // switch to extended Count
      d_back[0].rlePtr=(BYTE *)backItem; // as original ptr with 0-count
      ElemNode e;
      e.extendedCount = ITEM::maxCount()+1;
      d_data.push_back(e);
      d_back = &(d_data[d_data.size()-2]);
    } else
      d_back[0].extendedCount++; // add 1 to ptr
    return; // STOP
  }
  if (!shortCount && distance == (int)d_back[1].extendedCount){
    // add to "extendedCount"
    d_back[1].extendedCount++;
    return;
  }


  // clear the begin condition with 0 ptr if present
  if (d_data[0].rlePtr==0)
     d_data.clear();

  // add as new single ptr
  ElemNode e;
  e.rlePtr = ((BYTE *)elem)+1;
  d_data.push_back(e);
  d_back = &(d_data[d_data.size()-1]);
}


template<typename T>
size_t RLEPtrVector<T>::size() const {
  return d_nr;
}

template<typename T>
bool RLEPtrVector<T>::empty() const {
  return !d_nr;
}

//! debug
/*!
 *  is actual the size in bytes of storage
 *  misuse the fact that std::vector also has capacity
 */
template<typename T>
size_t RLEPtrVector<T>::capacity() const {
  return d_data.size()*4;
}

/* template<typename T>
 *  void RLEPtrVector<T>::dump() const {
 *   std::cout << "dump:\n";
 * if (d_data.empty())
 *   std::cout << " EMPTY\n";
 * for (size_t i=0; i < d_data.size(); i++) {
 *   std::cout << " " << i << " " << d_data[i].rlePtr << "\n";
 * }
 * }
 */

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
