#ifndef INCLUDED_PCRMODFLOW
#define INCLUDED_PCRMODFLOW

// This header first!
#ifndef INCLUDED_BOOST_PYTHON
#include <boost/python.hpp>
#define INCLUDED_BOOST_PYTHON
#endif

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif


// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

// Module headers.


// level = elevation level/pcraster map

class Common;
class GridCheck;
class BCF;
class RCH;
class BAS;
class DIS;
class SIP;
class PCG;
class RIV;
class DRN;
class DSP;
class SOR;
class WEL;
class HFB;


enum Solver {
  NO_SOLVER = 0,
  PCG_SOLVER,
  SOR_SOLVER,
  SIP_SOLVER,
  DSP_SOLVER
};

class PCRModflow : public dal::Client

{
/*
 fortran unit numbers:
   < 100: input files for the packages
 output:
 100 head
 101 bound
 140 bcf
 150 riv
 160 rch
 170 drn


*/

  friend class Common;
  friend class GridCheck;
  friend class BCF;
  friend class RCH;
  friend class BAS;
  friend class DIS;
  friend class SIP;
  friend class PCG;
  friend class RIV;
  friend class DRN;
  friend class DSP;
  friend class SOR;
  friend class WEL;
  friend class HFB;

 private:
  GridCheck *d_gridCheck;
  SIP *d_sip;
  RIV *d_riv;
  DIS *d_dis;
  BAS *d_bas;
  BCF *d_bcf;
  RCH *d_rch;
  PCG *d_pcg;
  DRN *d_drn;
  SOR *d_sor;
  DSP *d_dsp;
  WEL *d_wel;
  HFB *d_hfb;

  discr::Block *d_baseArea;
  /// \todo no longer used?
    discr::BlockData<REAL4> *d_baseLayer;
    // stores layer information
    discr::BlockData<REAL4> *d_layer;
    //
    discr::BlockData<INT4>  *d_ibound;
    discr::BlockData<REAL4> *d_initialHead;
    discr::BlockData<REAL4> *d_hCond;
    discr::BlockData<REAL4> *d_vCond;
    //
    discr::BlockData<REAL4> *d_rivStage;
    discr::BlockData<REAL4> *d_rivBottom;
    discr::BlockData<REAL4> *d_rivCond;
//                     discr::BlockData<REAL4> *d_rivLeakage;
    //
    discr::BlockData<REAL4> *d_recharge;
    discr::BlockData<INT4> *d_rechargeIrch;
//    discr::BlockData<REAL4> *d_rechargeResult;

    discr::BlockData<REAL4> *d_primaryStorage;
    discr::BlockData<REAL4> *d_secondaryStorage;
    discr::BlockData<REAL4> *d_wetting;

    discr::Raster *d_draster;

    discr::RasterData<REAL4> *d_thisbaseelev;
    // DRN
    discr::BlockData<REAL4> *d_drnElev;
    discr::BlockData<REAL4> *d_drnCond;
//                      discr::BlockData<REAL4> *d_drnResult;
    // WEL
    discr::BlockData<REAL4> *d_welValues;

    size_t d_nrMFLayer;
    size_t d_nrBlockLayer;

    int d_nrOfLayer;         // NLAY
    /// \todo rm Of
      size_t d_nrOfRows;          // NROW
      size_t d_nrOfColumns;       // NCOL
  size_t           d_nrOfCells;
      /// \todo one, double and rename to cellsize...
	float d_widthRows;
	float d_widthColumns;

	double d_cellsize;
	double d_west;
	double d_north;
	//


	std::vector<bool> d_quasiConfined;    // remove
	std::vector<int> d_layer2BlockLayer;  // this
	std::vector<int> d_layerType;         // three



	// the new layer numbering
	std::vector<bool> dd_isConfined;   // true if layer is quasi3D confining bed, false otherwise
	std::vector<int> dd_layerType;
	size_t dd_nrLayer;                 // contains the amount of internal layer
	size_t dd_nrModflowLayer;          // contains the amount of modflow layer (without confined)

	std::string modflow_directory;     // here mf will be executed
	std::string run_command;           // user specified execution command
	std::string run_arguments;         // user specified execution command


	/// \todo replace quasiconf level with vector cont layer

	  std::string d_methodName;
	  Common *d_cmethods;

	  bool d_isSteadyState;
	  bool d_baseElev;
	  // flag to test if one solver is specified
	  bool d_solver; /// 20151006 todo remove this
	  // flag to test if geometry changed
	  bool d_gridIsFixed;
	  // test if two confining layer are specified
	  /// \todo move to dis
	    bool d_lastIsConfined;


	    void initRIV();
	    void initDRN();
	    void initWEL();
	    size_t mfLayer2BlockLayer(size_t layer);


	    void initBlockData();
	    void initBlock(const discr::Block &elevation);
	    void initRCH(size_t option);


	    bool setBlockData(discr::BlockData<INT4> &bdata, const INT4 *values, size_t layer);
	    bool setBlockData(discr::BlockData<REAL4> &bdata, const REAL4 *values, size_t layer);

	    // methods writing the MODFLOW 2000 input files
	    bool writeNAM();
	    bool writeOC();

	    void resetGrid(bool final);
	    void printList();
	    void initREAL4BlockData(discr::BlockData<REAL4> **bdata, double init);

	    double d_startTime;
	    double d_endTime;
	    void initDataStructures();
  void removeTextFiles(std::string const & fileName) const;

  int              nr_internal_layer   () const;
  int              nr_modflow_layer    () const;
  size_t           get_modflow_layernr (size_t layer);
  void             modflow_converged   ();
  bool             d_modflow_converged;
  Solver           d_solver_used;
  std::string      run_directory       ();

public:
	    ~PCRModflow();

	    //PCRModflow();
	    PCRModflow(const geo::RasterSpace &raster);
	    PCRModflow(size_t rows, size_t cols, double cellsize, double xll, double yll);


	    // DIS
	    bool createBottom(const float *lower, const float *upper);
	    void createBottom(const calc::Field *lower, const calc::Field *upper);
	    bool addLayer(const float *values);
	    void addLayer(const calc::Field *values);
	    bool addConfinedLayer(const float *values);
	    void addConfinedLayer(const calc::Field *values);
	    void setLayer(const discr::Block &thickness, const discr::BlockData<INT4> &conf);

  void             set_row_width       (boost::python::list const& arguments);

  void             set_col_width       (boost::python::list const& arguments);


	    //
	    void setDISParams(size_t timeUnits, size_t lentghUnits, float stressPeriodLength, size_t nrOfTimesteps, float timeStepMultiplier, bool isSteadyState);

      void         update_dis_parameter(float stressPeriodLength, size_t nrOfTimesteps, float timeStepMultiplier);
	    // BAS
	    void setNoFlowConstant(float constVal);
	    bool setIBound(const int *values, size_t layer);
	    void setIBound(const calc::Field *values, size_t layer);
	    bool setIBound(const discr::BlockData<INT4> &values);
	    bool setInitialHead(const float *values, size_t layer);
	    void setInitialHead(const calc::Field *values, size_t layer);
	    bool setInitialHead(const discr::BlockData<REAL4> &values);
	    // BCF
	    bool setHCond(const float *values, size_t layer, size_t type);
	    void setHCond(const discr::BlockData<REAL4> &values, const discr::BlockData<INT4> &type);

	    bool setVCond(const float *values, size_t layer);
	    void setVCond(const discr::BlockData<REAL4> &values);
	    void setCond(size_t laycon, const calc::Field *hcond, const calc::Field *vcond, size_t layer);


	    void setTRPY(float trpy);
	    void setHDRY(float hdry);
	    void setWettingParameter(float wetfct, size_t iwetit, float ihdwet);
	    bool setWetting(const float *values, size_t mfLayer);
	    void setWetting(const calc::Field *values, size_t layer);
	    void setWetting(const discr::BlockData<REAL4> &values);

	    bool setPrimaryStorage(const float *values, size_t mfLayer);
	    bool setSecondaryStorage(const float *values, size_t mfLayer);
	    void setStorage(const calc::Field *primary, const calc::Field *secondary, size_t layer);
	    void setStorage(const discr::BlockData<REAL4> &primary, const discr::BlockData<REAL4> &secondary);
	    // Recharge package
	    void setRecharge(const float *values, size_t optCode);
	    void setRechargeLay(const float *rch, const int *layer);
	    void setRecharge(const calc::Field *rch, size_t optCode);
	    void setRechargeLay(const calc::Field *rch, const calc::Field *layer);
	    //bool setRecharge(const std::string &filename, size_t option);
	    // RIV package
	    bool setRiver(const float *rivH, const float *rivB, const float *rivC, size_t mfLayer);
	    void setRiver(const calc::Field *rivH, const calc::Field *rivB, const calc::Field *rivC, size_t layer);
	    void setRiver(discr::BlockData<REAL4> &stage, discr::BlockData<REAL4> &bottom, discr::BlockData<REAL4> &cond);
	    // DRN package
	    bool setDrain(const float *elevation, const float *conductance, size_t mfLayer);
	    void setDrain(const calc::Field *elevation, const calc::Field *conductance, size_t layer);
	    void setDrain(const discr::BlockData<REAL4> &elevation, const discr::BlockData<REAL4> &conductance);
	    // WEL package
	    bool setWell(const float *well, size_t mfLayer);
	    void setWell(const calc::Field *well, size_t layer);
	    void setWell(discr::BlockData<REAL4> &well);
	    // Solver packages
	    void setSOR(size_t mxiter, double accl, double hclose);
	    void setSIP(size_t mxiter, size_t nparam, double accl, double hclose, size_t ipcalc, double wseed);
	    void setPCG(size_t mxiter, size_t iteri, size_t npcond, double hclose, double rclose, double relax, double nbpol,  double damp);
	    void setDSP(size_t itmx, size_t mxup, size_t mxlow, size_t mxbw, size_t ifreq, double accl,  double hclose);

	    bool runModflow(const std::string & working_directory="");

      void set_run_command(const std::string & command, const std::string & arguments);

	    // retrieving the MODFLOW 2000 output
	    void getDrain(float *values, size_t mfLayer);
	    calc::Field* getDrain(size_t layer);
//	    discr::BlockData<REAL4>* getBlockDrain();

	    void getRecharge(float *result, size_t mfLayer);
	    calc::Field* getRecharge(size_t layer);
//	    discr::BlockData<REAL4>* getBlockRecharge();

	    void getHeads(float *result, size_t layer);
	    calc::Field* getHeads(size_t layer);
	    discr::BlockData<REAL4>* getBlockHeads();

	    void getRiverLeakage(float *result, size_t mfLayer);
//	    discr::BlockData<REAL4>* getBlockRiverLeakage();
	    calc::Field* getRiverLeakage(size_t layer);


  void             get_storage         (float *values,
                                        size_t mfLayer);

  calc::Field*     get_storage         (size_t layer);

  void             get_constand_head   (float *values,
                                        size_t mfLayer);

  calc::Field*     get_constand_head   (size_t layer);

  void             get_right_face      (float *values,
                                        size_t mfLayer);

  calc::Field*     get_right_face      (size_t layer);

  void             get_front_face      (float *values,
                                        size_t mfLayer);

  calc::Field*     get_front_face      (size_t layer);

  void             get_lower_face      (float *values,
                                        size_t mfLayer);

  calc::Field*     get_lower_face      (size_t layer);

  bool             converged           ();



  // workaround?
  void createBottomPS(const std::string & lower, const std::string & upper);
  void addLayerPS(const std::string & values);
  void addConfinedLayerPS(const std::string & values);


  void setIBound(const std::string & values, size_t layer);
  void setInitialHead(const std::string & values, size_t layer);
  void setCond(size_t laycon, const std::string & hcond, const std::string & vcond, size_t layer);
  void setRecharge(const std::string & , size_t optCode);
  void setRechargeLay(const std::string & , const std::string & );
  void setStorage(const std::string & , const std::string & , size_t layer);
  void setWetting(const std::string & values, size_t mfLayer);
  void setRiver(const std::string & rivH, const std::string & rivB, const std::string & rivC, size_t layer);
  void setDrain(const std::string & elevation, const std::string & conductance, size_t layer);
  void setWell(const std::string & well, size_t mfLayer);
};

#endif // INCLUDED_PCRMODFLOW
