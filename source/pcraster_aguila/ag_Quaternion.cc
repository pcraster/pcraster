#include "ag_Quaternion.h"
#include <cassert>
#include <cmath>



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::Quaternion::Quaternion()
{
  reset();
}



ag::Quaternion::Quaternion(const Quaternion& q)
{
  d_val[0] = q.d_val[0];
  d_val[1] = q.d_val[1];
  d_val[2] = q.d_val[2];
  d_val[3] = q.d_val[3];
}



ag::Quaternion::Quaternion(GLfloat x, GLfloat y, GLfloat z)
{
  Quaternion qx(x, 1.0, 0.0, 0.0);
  Quaternion qy(y, 0.0, 1.0, 0.0);
  Quaternion qz(z, 0.0, 0.0, 1.0);

  *this = qx;
  postMult(qy);
  postMult(qz);
}



ag::Quaternion::Quaternion(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
{
  // Normalize the axis.
  GLfloat factor = x * x + y * y + z * z;
  assert(factor != 0.0);

  GLfloat scale(1.0 / std::sqrt(static_cast<double>(factor)));
  x *= scale;
  y *= scale;
  z *= scale;

  // Build a quaternion.
  d_val[0] = std::cos(static_cast<double>(angle) / 2.0);

  GLfloat sinha = std::sin(static_cast<double>(angle) / 2.0);
  d_val[1] = x * sinha;
  d_val[2] = y * sinha;
  d_val[3] = z * sinha;
}



ag::Quaternion::~Quaternion()
{
}



ag::Quaternion& ag::Quaternion::operator=(const Quaternion& q)
{
  if(this != &q) {
    d_val[0] = q.d_val[0];
    d_val[1] = q.d_val[1];
    d_val[2] = q.d_val[2];
    d_val[3] = q.d_val[3];
  }

  return *this;
}



void ag::Quaternion::normalize()
{
  GLfloat factor = d_val[0] * d_val[0] + d_val[1] * d_val[1] +
                   d_val[2] * d_val[2] + d_val[3] * d_val[3];
  assert(factor != 0.0);
  GLfloat scale = 1.0 / std::sqrt(static_cast<double>(factor));
  d_val[0] *= scale;
  d_val[1] *= scale;
  d_val[2] *= scale;
}



void ag::Quaternion::reset()
{
  d_val[0] = 1.0;
  d_val[1] = 0.0;
  d_val[2] = 0.0;
  d_val[3] = 0.0;
}



void ag::Quaternion::matrix(GLfloat m[16])
{
  normalize();

  GLfloat w = d_val[0];
  GLfloat x = d_val[1];
  GLfloat y = d_val[2];
  GLfloat z = d_val[3];

  GLfloat xx = x * x;
  GLfloat yy = y * y;
  GLfloat zz = z * z;

  #define M(x, y)  m[x + y * 4]

  M(0, 0) = 1.0 - 2.0 * (yy + zz);
  M(1, 0) = 2.0 * (x * y + w * z);
  M(2, 0) = 2.0 * (x * z - w * y);
  M(3, 0) = 0.0;

  M(0, 1) = 2.0 * (x * y - w * z);
  M(1, 1) = 1.0 - 2.0 * (xx + zz);
  M(2, 1) = 2.0 * (y * z + w * x);
  M(3, 1) = 0.0;

  M(0, 2) = 2.0 * (x * z + w * y);
  M(1, 2) = 2.0 * (y * z - w * x);
  M(2, 2) = 1.0 - 2.0 * (xx + yy);
  M(3, 2) = 0.0;

  M(0, 3) = 0.0;
  M(1, 3) = 0.0;
  M(2, 3) = 0.0;
  M(3, 3) = 1.0;
}



void ag::Quaternion::postMult(const Quaternion& q)
{
  Quaternion tmp(*this);
  multAndSet(tmp, q);
}


void ag::Quaternion::multAndSet(const Quaternion& q1, const Quaternion& q2)
{
  d_val[0] =  q2.d_val[0] * q1.d_val[0]
            - q2.d_val[1] * q1.d_val[1]
            - q2.d_val[2] * q1.d_val[2]
            - q2.d_val[3] * q1.d_val[3];

  d_val[1] =  q2.d_val[0] * q1.d_val[1]
            + q2.d_val[1] * q1.d_val[0]
            + q2.d_val[2] * q1.d_val[3]
            - q2.d_val[3] * q1.d_val[2];

  d_val[2] =  q2.d_val[0] * q1.d_val[2]
            - q2.d_val[1] * q1.d_val[3]
            + q2.d_val[2] * q1.d_val[0]
            + q2.d_val[3] * q1.d_val[1];

  d_val[3] =  q2.d_val[0] * q1.d_val[3]
            + q2.d_val[1] * q1.d_val[2]
            - q2.d_val[2] * q1.d_val[1]
            + q2.d_val[3] * q1.d_val[0];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


