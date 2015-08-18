#ifndef INCLUDED_AG_TYPES
#define INCLUDED_AG_TYPES





namespace ag {

/*
typedef enum FileFormatId { PNG, EPS } FileFormatId;
*/

enum MapAction {
  QUERY,
  PAN,
  ZOOM_AREA,
  SELECT,
  NR_MAP_ACTIONS
};

enum DrawerType {
  COLOURFILL,
  CONTOUR,
  VECTORS,
  NR_DRAWER_TYPES
};

enum ViewerType {
  VT_Map,
  VT_Graph
};

} // namespace ag

#endif

