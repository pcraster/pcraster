#ifndef INCLUDED_CALC_AREAOPERATIONS
#define INCLUDED_CALC_AREAOPERATIONS

#include "stddefx.h"
#include "pcrtypes.h"
#include "calc_averagemap.h"


namespace calc {
  // AreaOperations declarations.
}



namespace calc {

//! implement areatotal and alikes
class AreaOperations
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  AreaOperations&           operator=           (AreaOperations const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   AreaOperations               (AreaOperations const& rhs);

protected:

  AreaAverageMap d_map;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AreaOperations               ();

  virtual          ~AreaOperations              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  template<typename IDF>
  void             apply                        (REAL4 *val,
                                                 const IDF   *id,
                                                 size_t len);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual double    statistic                    (INT4  idValue) const=0;

};

class AreaTotalOperation : public AreaOperations
{
  public:
    virtual ~AreaTotalOperation() = default;
    double     statistic                    (INT4  idValue) const override
    {
      return d_map.find(idValue)->second.sum();
    }
};

class AreaAverageOperation : public AreaOperations
{
  public:
    virtual ~AreaAverageOperation() = default;
    double     statistic                    (INT4  idValue) const override
    {
      return d_map.find(idValue)->second.average();
    }
};

class AreaMinimumOperation : public AreaOperations
{
  public:
    virtual ~AreaMinimumOperation() = default;
    double     statistic                    (INT4  idValue) const override
    {
      return d_map.find(idValue)->second.minimum();
    }
};

class AreaMaximumOperation : public AreaOperations
{
  public:
    virtual ~AreaMaximumOperation() = default;
    double     statistic                    (INT4  idValue) const override
    {
      return d_map.find(idValue)->second.maximum();
    }
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

#endif
