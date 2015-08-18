#ifndef INCLUDED_QT_DEF
#define INCLUDED_QT_DEF



namespace qt
{

  typedef unsigned int SideFlags;

  enum Side
  {
    Left   = 0x00000001,
    Top    = 0x00000002,
    Right  = 0x00000004,
    Bottom = 0x00000008
  };

  enum Orientation
  {
    Vertical,
    Horizontal
  };

  enum Corner
  {
    UpperLeft,
    UpperRight,
    LowerLeft,
    LowerRight
  };

  enum ApplicationRole
  {
    //! Application has full control over the process.
    StandAlone,

    //! Application is a client in the process and does not control it.
    Client

  };

}

#endif

