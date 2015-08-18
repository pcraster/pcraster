#include "stddefx.h"

#ifndef INCLUDED_CALC_BLOCKPOSITION
#include "calc_blockposition.h"
#define INCLUDED_CALC_BLOCKPOSITION
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
# include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_ALGORITHM
# include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

calc::BlockPosition::BlockPosition():
          d_nrChildren(0)
{
}

calc::BlockPosition::BlockPosition(calc::BlockPosition& parent):
          d_nrChildren(0)
{
          d_key = parent.d_key;
    d_key.push_back(parent.d_nrChildren++);
}

size_t calc::BlockPosition::grade(const calc::BlockPosition& other) const
{
  size_t i,n = std::min(d_key.size(),other.d_key.size());
  for (i =0; i < n; i++) {
   if (d_key[i] != other.d_key[i])
     break;
  }
  return d_key.size()-i;
}

std::string calc::BlockPosition::asString() const
{
  std::ostringstream stream;
  stream << "Block id:";
  for (size_t i=0; i < d_key.size(); i++) 
    stream << d_key[i] << "-";
  return stream.str();
}
