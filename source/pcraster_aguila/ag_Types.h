#ifndef INCLUDED_AG_TYPES
#define INCLUDED_AG_TYPES

#include <cstdint>



namespace ag {

/*
typedef enum FileFormatId { PNG, EPS } FileFormatId;
*/

enum MapAction : std::uint8_t {
  QUERY,
  PAN,
  ZOOM_AREA,
  SELECT,
  NR_MAP_ACTIONS
};

enum DrawerType : std::uint8_t {
  COLOURFILL,
  CONTOUR,
  VECTORS,
  NR_DRAWER_TYPES
};

enum ViewerType : std::uint8_t {
  VT_Map,
  VT_Graph
};

} // namespace ag

#endif

