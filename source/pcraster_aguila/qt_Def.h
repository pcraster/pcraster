#ifndef INCLUDED_QT_DEF
#define INCLUDED_QT_DEF

#include <cstdint>


namespace qt
{

  using SideFlags = unsigned int;

  enum Side : std::uint8_t
  {
    Left   = 0x00000001,
    Top    = 0x00000002,
    Right  = 0x00000004,
    Bottom = 0x00000008
  };

  enum Orientation : std::uint8_t
  {
    Vertical,
    Horizontal
  };

  enum Corner : std::uint8_t
  {
    UpperLeft,
    UpperRight,
    LowerLeft,
    LowerRight
  };

  enum ApplicationRole : std::uint8_t
  {
    //! Application has full control over the process.
    StandAlone,

    //! Application is a client in the process and does not control it.
    Client

  };

}

#endif

