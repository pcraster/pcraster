#define DEVENV_NO_WARNINGS

#if defined(DEVENV_NO_WARNINGS)
  // Only add warnings which we really don't have to see.
  // Prepend each pragma with the warning message.

  #if defined(_MSC_VER)
    #if _MSC_VER

      // class 'A' needs to have dll interface for to be used by clients of
      // class 'B'.
      #pragma warning(disable: 4251)

      // non - DLL-interface classkey 'identifier' used as base for
      // DLL-interface classkey 'identifier'.
      #pragma warning(disable: 4275)

    #endif
  #endif
#endif
