#include "stddefx.h"
#include "calc_blockposition.h"
#include "calc_infoscript.h"
#include <algorithm>
#include <sstream>

calc::BlockPosition::BlockPosition()
{
}

calc::BlockPosition::BlockPosition(calc::BlockPosition& parent)
{
          d_key = parent.d_key;
    d_key.push_back(parent.d_nrChildren++);
}

size_t calc::BlockPosition::grade(const calc::BlockPosition& other) const
{
  size_t i = 0;
  size_t n = std::min(d_key.size(),other.d_key.size());
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
  for (unsigned long i : d_key) 
    stream << i << "-";
  return stream.str();
}
