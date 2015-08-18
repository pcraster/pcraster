#ifndef INCLUDED_CALC_FIELDVALUE
#define INCLUDED_CALC_FIELDVALUE

#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class FieldParameter;
class Position;

//! field value owned by a parameter
class  FieldValue {
protected:
  //! owned by d_par
  const FieldParameter&      d_par;
  //! index within d_par
  const size_t               d_index;
  //! info on how to write
  const FileWriter           d_fw;

  FieldHandle               *d_val;

  //! used for initialization of computed parameter
  FieldValue(const FieldParameter& p,size_t index, Field *val);

  //! used for initialization of new parameter
  FieldValue(const FieldParameter& p,size_t index);

  const Field *value() const;

  virtual void write()=0;
public:
  virtual ~FieldValue();

  void assign(FieldHandle e, const Position *assignPoint);

  FieldHandle value(bool isLastUse);
};

}

#endif
