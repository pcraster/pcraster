#ifndef INCLUDED_CALC_WRITEINFO
#define INCLUDED_CALC_WRITEINFO

namespace calc {
class IScript;
class Report;

//! could result of statement be written here (to disk)
/*! the statement has an explicit report prefix or is written
    here because of the default rule for non-dynamic scripts.
    This does not mean it <B>is</B> written here, since by the
    default rule of non-dynamic scripts, we only write the last
    assignment
 */
class WriteInfo {
 private:
  //! need for AllIsWritten and reportdefault
    const IScript* d_script;
  //! if the report keyword is explicit
  bool d_hasReportPrefix;
  //! 0 if not written
   const class Report *d_report;
  //! is the report in the dynamic section?
  bool d_inDynamic;
 public:
  // CREATORS
    WriteInfo( const IScript* script,
  		bool hasReportPrefix,
  		const Report *report,
  		bool inDynamic); 
  // MANIPULATORS

  // ACCESSORS
  const Report *report() const;

  //! must something be written at one or more timesteps
  bool isWritten() const;

  //! write at timestep t ?
  bool writeAtTimestep(size_t t) const;

  //! the statement contains an explicit report keyword
  bool hasReportPrefix() const;

  //! this is in the dynamic section
  bool inDynamic() const;
};

}

#endif
