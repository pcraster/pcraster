#ifndef INCLUDED_GEO_CSFSTACKNAME
#define INCLUDED_GEO_CSFSTACKNAME



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif



namespace geo {
  class CSFStackNamePrivate;
}



namespace geo {


/*!
  \brief The CSFStackName class is for objects which know which timesteps are
         available in a stack and what their names are.
  \warning This class will be replaced by dal::StackInfo, contact Kor if you
  are about to make changes to this class
  \todo
     document and explain difference in behavior of the 2 tests in test unit
     dealing with tmp.res stack name
*/
class CSFStackName
{

private:

  CSFStackNamePrivate *d_data;

  //! The stack is scanned for available timesteps or not.
  bool             d_scanned;

  //! If the stack is scanned, than this variable contains the available steps.
  std::vector<size_t> d_steps;

  void             init                (const com::PathName &n);

  void             clean               ();

  bool             isMemberOfStack     (const com::PathName& pathName) const;

  void             stackNamePool       (std::vector<com::PathName>& pool) const;

  bool             findTimeStep        (std::vector<com::PathName>& pool,
                                        size_t& step) const;

  void             firstAvailableTimeStep
                                       (std::vector<com::PathName>& pool,
                                        size_t& step) const;

  void             availableTimeSteps  (std::vector<com::PathName>& pool,
                                        std::vector<size_t>& steps) const;

public:

  //! Type of the iterator for timesteps.
  typedef std::vector<size_t>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CSFStackName        (const com::PathName& n,
                                        bool scanIt = true);

                   CSFStackName        (const com::PathName& pn,
                                        size_t f,
                                        size_t l,
                                        bool scanIt = true);

                   CSFStackName        (const CSFStackName& n);

                   ~CSFStackName       ();

  CSFStackName &   operator=           (const CSFStackName& rhs);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             scan                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static std::string asAguilaArgument  (std::string stackName,
                                        int start,
                                        int stop);

  size_t           nrLayers            () const;

  com::PathName    baseName            () const;

  com::PathName    fileName            () const;

  com::PathName    fileName            (size_t t) const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

  bool             scanned             () const;

  bool             isDynamic           () const;

  bool             isAvailable         (size_t t) const;

#ifdef DEBUG_DEVELOP
  friend std::ostream &operator<<      (std::ostream &s,
                                        const CSFStackName &n);
#endif

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace geo

#endif
