#ifndef INCLUDED_BCF
#define INCLUDED_BCF

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif


// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif


// Module headers.


class PCRModflow;

class BCF{
protected:
private:
  double           d_iwdflg;
  double           d_wetfct;
  double           d_ihdwet;
  double           d_trpy;
  size_t           d_iwetit;
  float            d_hdry;
  int              d_output_unit_number;
  int              d_hy_unit_number;
  int              d_vcond_unit_number;
  int              d_tran_unit_number;
  int              d_sf1_unit_number;
  int              d_sf2_unit_number;
  int              d_wet_unit_number;
  PCRModflow       *d_mf;

  bool hasConfinedSubLayer(size_t layer);
  void calcTran(std::stringstream &aStream, size_t layer, const std::string &msg);
  void calcVCond(std::stringstream &aStream, size_t layer, const std::string &msg);


  size_t getLaycon(size_t lcon);

//  void             calcSf1             (std::stringstream &aStream, size_t layer, const std::string &msg) const;

public:
  void             get_binary          (float *values, const std::string, size_t type,
                                        size_t layer, std::string const& path) const;
                   ~BCF                ();
                   BCF                 (PCRModflow *mf);

  void             writeBCF();

  void             setCond             (size_t laycon, const calc::Field *hcond, const calc::Field *vcond, size_t layer);
  void             setHCond            (const discr::BlockData<REAL4> &values, const discr::BlockData<INT4> &type);
  void             setVCond            (const discr::BlockData<REAL4> &values);
  void             setHDRY             (float hdry);
  void             setTRPY             (float trpy);
  void             setWetting          (const discr::BlockData<REAL4> &values);
  void             setWetting          (const calc::Field *values, size_t layer);
  void             setStorage          (const calc::Field *primary, const calc::Field *secondary, size_t layer);
  void             setStorage          (const discr::BlockData<REAL4> &primary, const discr::BlockData<REAL4> &secondary);
  void             setWettingParameter (float wetfct, size_t iwetit, float ihdwet);

  double           getHDRY             () const;

  void             get_storage         (float *values,
                                        size_t mfLayer, std::string const& path) const;

  calc::Field*     get_storage         (size_t layer, std::string const& path) const;

  void             get_constand_head   (float *values,
                                        size_t mfLayer, std::string const& path) const;

  calc::Field*     get_constand_head   (size_t layer, std::string const& path) const;

  void             get_right_face      (float *values,
                                        size_t mfLayer, std::string const& path) const;

  calc::Field*     get_right_face      (size_t layer, std::string const& path) const;

  void             get_front_face      (float *values,
                                        size_t mfLayer, std::string const& path) const;

  calc::Field*     get_front_face      (size_t layer, std::string const& path) const;

  void             get_lower_face      (float *values,
                                        size_t mfLayer, std::string const& path) const;

  calc::Field*     get_lower_face      (size_t layer, std::string const& path) const;

  void             write               (std::string const& path);

  void             write_hy    (std::string const& path);
  void             write_sf1    (std::string const& path) ;
  void             write_sf2    (std::string const& path);
  void             write_vcond    (std::string const& path);
  void             write_tran    (std::string const& path);
  void             write_wetdry    (std::string const& path);

  bool             transient           () const;

  bool             rewetting           () const;


  int              hy_unit_number() const;
  int              vcond_unit_number() const;
  int              wet_unit_number() const;
  int              tran_unit_number() const;
  int              sf1_unit_number() const;
  int              sf2_unit_number() const;
};

#endif // INCLUDED_BCF
