! Time of File Save by ERB: 6/16/2006 11:22AM
C   KJH  20030327  -- Patched Hyd.K term in LPF option -- cel2wel function
C   KJH  20030717  -- Patched budget output switch -- subroutine GWF1MNW1bd
c                     Cleaned output so outrageous pointers are not printed
c   GZH  20050405  -- Converted calculations to use double precision
c   KJH  20050419  -- Array WELL2 dimensioned to 18 to store well id
c   RTH  20060221  -- Fixed Variable declaration & constant declarations
c                     Fixed variable declaration errors in bd subroutine
c   ERB  20060308  -- Completed conversion to double precision
c                     Moved PLoss from argument lists to common block rev23
c   LJK  20060406  -- Edited GWF1MNW1BD to use well2(17,m) (which contains
c                     the cell-by-cell flows) rather than well2(3,m) (which
c                     contains the entire well value) in the call to UBDSVB
c   ERB  20060616  -- Added PERTIM to argument list of GWF1MNW1BD because
c                     it's needed in call to UBDSV4
      SUBROUTINE GWF1MNW1DF(LCHANI,LCHK,LCHKCC,LCHUFTHK,LCHY,LCSSHMN,
     &                      LCTRPY,NHUFAR)
C     VERSION 20020129 ERB
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY MNW1 TO SUPPORT MULTIPLE FLOW
C     PACKAGES
C     ******************************************************************
      IMPLICIT NONE
      INTEGER LCHANI, LCHK, LCHKCC, LCHUFTHK, LCHY, LCSSHMN, LCTRPY,
     &        NHUFAR
      LCHANI = 1
      LCHK = 1
      LCHKCC = 1
      LCHUFTHK = 1
      LCHY = 1
      LCSSHMN = 1
      LCTRPY = 1
      NHUFAR = 1
      RETURN
      END
c
c-------------------------------------------------------------------------
c
      SUBROUTINE GWF1MNW1al(isum, lcwel2, mxwel2, nwell2, lchref,
     +       nodes, kspref, in, iout, iwl2cb,iowell2, NoMoIter,
     +        MNWname, Fname)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     allocate array storage for well package
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      double precision BS, PLOSS, rn
      INTEGER NOMOITER, IWL2CB, IOUT, IN, KSPREF, NODES, LCHREF, NWELL2,
     &        MXWEL2, LCWEL2, ISUM, IWELPT, ICF, IOWELL2, IERR, KI,
     &        IFRL, KF, KE, IOK, KIO, JF, ISP, IO
      dimension rn(25), iowell2(3), icF(3)
      character*6 ftag(3)
      character*200 FNAME, MNWname                          !!08/19/02KJH-MODIFIED
      character*256 txt, tx2
      data ftag/'WEL1  ','BYNODE','QSUM  '/
      data  icF/4,6,4/
c
      iowell2(1) = 0
      iowell2(2) = 0
      iowell2(3) = 0
c
c1------identify package and initialize nwell2
      write(iout,1)in
    1 format(/,1x,'MNW1 -- MULTI-NODE WELL PACKAGE, VERSION 1,',
     +' 8/13/2002.',/,4X,'INPUT READ FROM UNIT ',i3)
      nwell2=0
c
c2------read max number of wells and
c2------unit or flag for cell-by-cell flow terms.
      call ncread(in,txt,ierr)
      call UPCASE(txt)
c
      ki = index(txt,'REF')
      if( ki.gt.0 ) then
        tx2 = txt(ki:256)
        call qread(rn,1,tx2,ierr)
        if( ierr.eq.0 ) kspref = ifrl( rn(1) )
        txt(ki:256) = '                                '
      else
        kspref = 1
      endif
c
      call qread(rn,4,txt,ierr)
      mxwel2 = ifrl( rn(1) )
      iwl2cb = 0
      if(ierr.le.2) iwl2cb = ifrl( rn(2) )
      iwelpt = 0
      if(ierr.eq.1) iwelpt = ifrl( rn(3) )
      NoMoIter   = 9999
      if(ierr.eq.0) NoMoIter = ifrl( rn(4) )
c
      write(iout,3) mxwel2
    3 format(1h ,'MAXIMUM OF',i5,' WELLS')
      if(iwl2cb.gt.0) write(iout,9) iwl2cb
    9 format(1x, 'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
      if(iwl2cb.lt.0) write(iout,8)
    8 format(1x,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      write(iout,'(2x,33hThe heads at the beginning of SP:,i4,
     +      1x,41hwill be the default reference elevations.,/)') kspref
      write(iout,7) NoMoIter
    7 format(1x,'Flow rates will not be estimated after the',i4,'th',
     +          ' iteration')
c
c   Define well model to be used
c
      call ncread(in,txt,ierr)
      call UPCASE(txt)
      PLoss = 0.0D0   !!  Default use of Skin so linear loss varies with T
      if( index(txt,'LINEAR').gt.0 ) then
        PLoss = 1.0D0 !!  ADD THIS LINE to make sure that the power term is 1 for the linear model
        ki = index(txt,':') + 1
        tx2 = txt(ki:256)
        call qread(rn,1,tx2,ierr)
        if(ierr.eq.0) PLoss = rn(1)
c   Add error checking to shut down MODFLOW
        BS = 3.6           !!   Maximum limit on power term
        if( PLoss .gt. BS ) then
          write(*,*)'Power term of',PLoss,' exceeds maximum of', BS
          write(iout,*)'Power term of',PLoss,' exceeds maximum of',BS
C
C         When compiling MNW with Modflow-96, comment out the call to
C         USTOP and uncomment the STOP statement
          CALL USTOP(' ')
C          STOP
C
        endif
c
      endif
c
c   Test for a specified PREFIX NAME  for time series output from MNW1OT
c
      call ncread(in,txt,ierr)
      tx2 = txt
      call UPCASE(tx2)
      kf = index(tx2,'PREFIX:')
      if( kf.gt.0 ) then
        MNWname = txt(kf+7:256)
        ke = index(MNWname,' ')
        MNWname(ke:200) = '               '
        tx2 = MNWname
        call UPCASE(tx2)
        if( index(tx2,'FILEPREFIX').gt.0 ) then
          MNWname = Fname
          ke = index(MNWname,'.')
          MNWname(ke:200) = '               '
        endif
      else
        MNWname = 'OUTput_MNW'
        backspace(in)
      endif
c
c     Test for creation of a WEL1 package and auxillary output files
c
      iok = 1
      do while( iok.eq.1 )
        call ncread(in,txt,ierr)
        tx2 = txt
        call UPCASE(tx2)
        kf = index(tx2,'FILE:')
        if( kf.gt.0 ) then
          kio = 0
          jf = 0
          do while( kio.eq.0 .and. jf.lt.3 )
            jf = jf + 1
            kio = index(tx2,ftag(jf)(1:icF(jf)))
            if( kio .gt.0 ) then
              tx2 = txt(kio+1+icF(jf):256)
              call qread(rn,1,tx2,ierr)
              if( ierr.eq.0 ) then
                iowell2(jf) = ifrl( rn(1) )
c            OC over ride is ALLTIME
                if(index(tx2,'ALLTIME').gt.0) iowell2(jf) = -iowell2(jf)
c            Find and use file name
                tx2 = txt(kf+5:256)
                kf = index(tx2,' ') - 1
                close( abs(iowell2(jf)) )
                open(abs(iowell2(jf)), file=tx2(1:kf) )
                write(tx2(253:256),'(i4)') abs(iowell2(jf))
                txt =' A '//ftag(jf)//' data input file will be written'
     +          //' to '//tx2(1:kf)//' on unit '//tx2(253:256)
                write(iout,'(/1x,a79)') txt
                if( jf.eq.1 )
     +          write(abs(iowell2(jf)),'(3i10)') mxwel2, iwl2cb, iwelpt
              endif
            endif
          enddo
        else
          backspace(in)
          iok = 0
        endif
      enddo
c
c3------set lcwel2 equal to location of well list in x array.
      lcwel2 = isum
c
c4------add amount of space used by well list to isum.
cerb      isp  = 16 * mxwel2
cerb  Change made 7/11/2003 - ERB
ckjh      isp  = 17 * (mxwel2 + 1)    !!7/13/2003 - CZ: increased to 17 from 16
      isp  = 18 * (mxwel2 + 1)    !!4/18/2005 - KJH:  Explicit well tracking addition
      isum = isum+isp
c  set aside a single precision array for a set of reference heads
      lchref = isum
      isp  = nodes
      isum = isum+isp
c
c5------print number of spaces in x array used by well package.
      write(iout,4) isp
    4 format(1x,i6,' ELEMENTS IN X ARRAY ARE USED FOR MNW1')
c
c  Write header in Auxillary BYNODE file if KPER=1 & IO>0
c
      if ( iowell2(2).ne.0 ) then
        io = abs(iowell2(2))
        write(io,'(6hSiteID,26x,6h Entry,6h  NODE,5x,10hTotal_Time,
     +        8x,1hQ,5x,6hH-Well,5x,6hH-Cell,5x,6hQW-Avg)')
      endif
c
c  Write header in Auxillary QSUM file if KPER=1 & IO>0
c
      if ( iowell2(3).ne.0 ) then
        io = abs(iowell2(3))
        write(io,'(6hSiteID,30x,6h Entry,5x,10hTotal_Time,
     +     10x,3hQin,10x,4hQout,10x,4hQsum,5x,6hH-Well,5x,6hQW-Avg)')
      endif
c
c7------return
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF1MNW1RP(MNWsite,well2,nwell2,mxwel2,hold,href,
     +  ibound,delr,delc,cr,cc,hy,hnew,hclose,small,Hdry,nodes,nrow,
     +  ncol,kper,kspref,in,iout,iowell2,totim,LAYHDT,BOTM,NBOTM,HK,
     &  IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     read new well locations, stress rates, conc, well char., and limits
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      COMMON /BCFCOM/LAYCON(999)
      INTEGER MXWEL2, NODES, IBOUND, NCOL, NROW, NBOTM, NLAY, LAYHDT, M,
     &        K, J, I, IRMX, NODE, IOK, IPT, IUHUF, IULPF, IUBCF, IOUT,
     &        IN, IOWELL2, IWELPT, LAYCON, NQREJECT, NL, N, IERR,
     &        KSPREF, KPER, NWELL2, ITMP, NSTART, IFRL, KQC, KCP, KPC,
     &        KSITEID, KBLK, KTAB, KFINI, KI, IP, N1, MSTEP, IDIRECT,
     &        NN, ICMN, NGRP, NE, NB, IIN, IDWELL, MM, II, IO, IGRP
      DOUBLE PRECISION PLOSS, Q
      REAL TOTIM, HOLD, DELR, DELC, CR, CC, HY, BOTM, HANI, HK, HKCC,
     *     TRPY, HDRY, HCLOSE
      double precision well2,href,rn
      double precision zero,Qfrcmn,Qfrcmx,Qreject,small,hmax,DryTest,
     * ipole,hlim,hrfw,qsum,rw,cond,Qact,sk,Cf
      dimension MNWsite(mxwel2)
      dimension well2(18,mxwel2+1),hold(nodes),href(nodes),ibound(nodes)
      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
      dimension hy(nodes)
      dimension rn(25), iowell2(3)
      dimension hnew(nodes)
      double precision hnew
      character*1  tab
      character*32 MNWsite, TempSite
      CHARACTER*200 FNAME, MNWname                          !!08/19/02KJH-MODIFIED
      character*256 txt, tx2, txtRAW
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
     &          LAYHDT(NLAY), TRPY(NLAY)
      double precision cel2wel
      SAVE HMAX
C
      tab = char(9)
      zero = 1.D-25
      Qfrcmn = zero
      Qfrcmx = zero
      Qreject  = 0.00000000000D0
      NQreject = 0
      NL = 0
      if( PLoss.gt.1.001D0 ) NL = 1  !!  Read NL loss Coefficient after Skin
      small = hclose
c
c  Check for setting the HREFerence array
CERB     IN FIRST STRESS PERIOD, HOLD IS UNDEFINED, SO USE HNEW INSTEAD
      IF (KPER.EQ.1) THEN
        hmax = HNEW(1)
        do n = 1, nodes
          href(n) = HNEW(n)
          if( abs(href(n)).gt.hmax ) hmax = abs(href(n))
        enddo
      ELSEif( kper.le.kspref ) then
        hmax = hold(1)
        do n = 1, nodes
          href(n) = hold(n)
          if( abs(href(n)).gt.hmax ) hmax = abs(href(n))
        enddo
      endif
c
c------------------------------------------------------------------
c     The 18 rows of the well array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = Well node locator
c         2   = Desired flow rate
c         3   = Actual flow rate used
c         4   = Water Quality attribute to be averaged by flow
c         5   = Radius of wellbore
c         6   = Skin associated with well completion
c         7   = Minimum/Maximum head or drawdown
c         8   = Elevation of reference head for computing lift costs
c         9   = Water Quality Group identifier
c        10   = Water level in wellbore
c        11   = HCOF value / QWaverage
c        12   = RHS  value
c        13   = Minimum flow rate -  to turn off
c        14   = Minimum flow rate -- to turn on
c        15   = Reserve Desired flow rate
c        16   = Non-linear loss term
c        17   = Actual flow rate to individual nodes of a multi-node well
c               kept for transport or other purposes !!7/13/2003 - CZ
c        18   = Explicit well identifier -- Same value for all nodes in a well
c------------------------------------------------------------------
c
c1------read itmp(number of wells or flag saying reuse well data)
      call ncread(in,txtRAW,ierr)
      txt = txtRAW
      call UPCASE(txt)
      call qread(rn,1,txt,ierr)
      itmp  = rn(1)
c
      if( itmp.lt.0 ) then
c        if itmp less than zero reuse data. print message and return.
        write(iout,6)
    6   format(1h0,'REUSING MNW1  FROM LAST STRESS PERIOD')
        return
      else
c  If itmp > 0,  Test if wells are to replace old ones or be added.
c
        if( index(txt,'ADD').eq.0 ) nwell2 = 0
c
c   return if there are no wells to read ........
        if( itmp .eq. 0 ) return
c
c  Redundant well information is allowed in MNW1
c
c   Read additional well info
        nstart = nwell2
        do m = 1, itmp
          call ncread(in,txtRAW,ierr)
          txt = txtRAW
          call UPCASE(txt)
c   Attempt read with QREAD first
          call qread(rn,4,txt,ierr)
          if( ierr.eq.0 .and. rn(5).lt.0.5D0 ) then
            k = ifrl( rn(1) )
            j = ifrl( rn(2) )
            i = ifrl( rn(3) )
            q = rn(4)
            irmx = ifrl( rn(6) ) + 1
          else
c  Use fixed form reader if errors were detected
            read (txt(1:40),'(3i10,f10.0)') k, j, i, q
            irmx = 41
          endif
          node = (k-1)*ncol*nrow + (j-1)*ncol + i
c    Test for if well is in active grid ......
          iok = 1
          if(i.gt.ncol .or. j.gt.nrow .or. node.gt.nodes) iok = 0
          DryTest = Hnew(node) - Hdry
          if( iok.gt.0 .and. ABS(DryTest).gt.zero) iok = ibound(node)
c
c  Should MNW wells be allowed in specified-head cells?
          if( iok .ne. 0 ) then     !! Allow SH now, "gt" for no SH
c    Test for redundant info ......
            ipt = 0
c    The commented statements prevent having multiple MNW sites in the same cells
c            nt  = 0
c            do while (nt.lt.nwell2 .and. ipt.eq.0 )
c              nt = nt + 1
c              if( well2(1,nt).eq.node ) ipt = nt
c            enddo
            if( ipt.eq.0 ) then
              nwell2 = nwell2 + 1
              ipt    = nwell2
            endif
c
c    Assign data now that the pointer is set
            well2(1,ipt) = node
            well2(2,ipt) = q
            IPOLE = 0
            if( abs(q).gt.zero )  ipole = q / abs(q)
            well2(3,ipt) = well2(2,ipt)
            well2(13,ipt) = Qfrcmn        ! default lower limit
            well2(14,ipt) = Qfrcmx
c
c    Look for limit modifications
            kqc = index(txt,'QCUT')
            kpc = index(txt,'%CUT')
            if( kqc+kpc.gt.0 .and. abs(q).gt.zero) then
              tx2 = txt(kqc+kpc+5:256)
              call qread(rn,2,tx2,ierr)
              if( kqc.gt.0 ) then          !!  Absolute value was provided
                rn(1) = 100.D0 * rn(1) / q    !!  Convert to percentage
                rn(2) = 100.D0 * rn(2) / q
              endif
              if( ierr.ge.1 ) rn(2) = rn(1)
              well2(13,ipt) = rn(1) * 0.01D0   !! convert percentages to fractions
              well2(14,ipt) = rn(2) * 0.01D0
              if( index(tx2,'DEFAULT').gt.0 ) then
                Qfrcmn = rn(1) * 0.01D0        !!  New default lower limit
                Qfrcmx = rn(2) * 0.01D0        !!  New default upper limit
              endif
            endif
c
c    Look for NonLinear coefficient
            well2(16,ipt) = 0.000000D0      !!  NonLinear Loss Coefficient
            kCp = index(txt,'CP:')
            if( kCp.gt.0 .and. NL.gt.0 ) then
              tx2 = txt(kCp+3:256)
              call qread(rn,1,tx2,ierr)
              if( ierr.eq.0 ) then
                well2(16,ipt) = rn(1)
c         Could reset default C-term here to a non-zero value
              endif
            endif
c
c   Look for Site Identifier   -- Set to NO-PRINT  if not present.
            kSiteID = index(txt,'SITE')
            if( kSiteID.gt.0 ) then
              MNWsite(ipt) = txtRAW(kSiteID+5:256)
              kblk = index(MNWsite(ipt),' ')
              ktab = index(MNWsite(ipt),tab)
              if( kblk.gt.0 ) Kfini = kblk
              if( ktab.gt.0 .and. ktab.lt.kblk) Kfini = ktab
              if(Kfini.le.32) then
                MNWsite(ipt)(kFini:32)='                 '
              else
                Kfini = 32
              endif
              txt(kSiteID:kSiteID+kFini+4) = '                        '
            else
              MNWsite(ipt) = 'NO-PRINT                     '
            endif
c
c    Read remaining info from card to set MNW1 specific parameters
            tx2 = txt(irmx:256)
            ki = index(tx2,'ZONE')
            if(ki.gt.0 )  tx2(ki:256)= '                         '
            call qread(rn,6,tx2,ierr)
c
c   Move from well data from temp to permanent locations
            do ip = 1, 6-ierr
              well2(ip+3,ipt) = rn(ip)
            enddo
            if( ierr.ge.1 ) well2(9,ipt) = ipt
            if( ierr.ge.2 .or. abs(well2(8,ipt)).gt.hmax )
     +          well2(8,ipt) = href(node)
c  Compute HLIM relative to reference elevation if HLIM read was a DrawDown (DD)
            if( index(txt,'DD').gt.0 )
     +        well2(7,ipt) = ipole*well2(7,ipt) + well2(8,ipt)
            if( ierr.ge.3 ) well2(7,ipt) = ipole * 1.0D+26
            if( ierr.ge.4 ) well2(6,ipt) =  0.0000D0
            if( ierr.ge.5 ) well2(5,ipt) =  0.0000D0
            if( ierr.ge.6 ) well2(4,ipt) = -1.0000D0
c  Flag as 2-point definition of a multi-node well if MULTI is detected.
            if( index(tx2,'MULTI') .gt. 0  .and.
     +          abs(well2(5,ipt))  .gt. zero  ) then
c  Define direction and # of points in well
              well2(2,ipt-1) = well2(2,ipt) + well2(2,ipt-1)
              n1 = ifrl( well2(1,ipt-1) )
              mstep = idirect( n1, node, ncol, nrow )
              do nn = n1+mstep, node, mstep
                ipt = ipt + 1
                nwell2 = nwell2 + 1
                well2(1,ipt) = nn
                well2(2,ipt) = 0.0000D0
                well2(3,ipt) = well2(2,ipt)
                well2(4,ipt) = well2(4,ipt-1)
                well2(5,ipt) = well2(5,ipt-1)
                well2(6,ipt) = well2(6,ipt-1)
                well2(16,ipt)= well2(16,ipt-1)  !!  NonLinear Loss Coefficient
                well2(9,ipt) = well2(9,ipt-1)
                well2(8,ipt) = -1.0D31
                well2(13,ipt) = 0.0000D0
                well2(14,ipt) = 0.0000D0
                icmn = icmn + 1
                well2(7,ipt) = icmn
              enddo
c  Flag as part of a multi-node well if MN is detected.
            elseif( index(tx2,'MN')  .gt. 0 .and.
     +              abs(well2(5,ipt)).gt.zero) then
c  Set to very large -value to flag MN status
                well2(8,ipt) = -1.0D31
              icmn = icmn + 1
                well2(7,ipt) = icmn
            else
              icmn = 1
            endif
          else
c   Sum details on rejected wells
            Qreject  = Qreject  + q
            NQreject = NQreject + 1
          endif    !   IBOUND test statement
        enddo      !   end of well entry loop
c
c   Process wells that are screened across multiple nodes
c
c Check for extreme contrast in conductance
c
        well2(8,nwell2+1) = 0.0000D0
        if( nstart.lt.1 )      nstart = 1
        if( nstart.gt.nwell2 ) nstart = nwell2 - itmp + 1
        do i = nstart, nwell2
          if( well2(8,i).lt.-1.D30 .and. well2(8,i+1).gt.-1.D30 .or.
     +        well2(8,i).lt.-1.D30 .and. i.eq.nwell2          ) then
            ngrp = ifrl( well2(7,i)      )
            ne   = i
            nb   = ne - ngrp + 1
            hlim = well2(7,nb)
            hrfw = well2(8,nb)
            qsum = 0.0000D0
            TempSite = 'NO-PRINT                     '
            do iin = nb, ne
              qsum = qsum + well2(2,iin)
              if( MNWsite(iin)(1:8).ne. 'NO-PRINT' ) then
                TempSite = MNWsite(iin)
              endif
              well2(2,iin) = 0.000000D0
              well2(7,iin) = 1.0D31
              well2(8,iin) = 1.0D31
c   Set to very large +value to flag MN status
            enddo
c   Set All SiteIDs in a multinode well to a common tag
            do iin = nb, ne
              MNWsite(iin) = TempSite
            enddo
            well2(7,nb) = ne
            well2(2,ne) = qsum
            well2(7,ne) = hlim
            well2(8,ne) = hrfw
          endif
        enddo   !   end of multi-well pointer setting
c
      endif
c
c  nwell2>mxwel2.  print message. stop.
      if( nwell2 .gt. mxwel2) then
        write(iout,99) nwell2, mxwel2
   99   format(1h0,'nwell2(',i4,') IS GREATER THAN mxwel2(',i4,')')
C
C       When compiling MNW with Modflow-96, comment out the call to
C       USTOP and uncomment the STOP statement
        CALL USTOP(' ')
C        STOP
C
      endif
c
c   Place desired flow rates in a reserved location
      do m = 1, nwell2
        well2(15,m) = well2(2,m)
      enddo
c
c--assign unique well id for use with MT3DMS link package (cdl: 4/19/05)
        m=0
        IDwell=1
        do while (m.lt.nwell2)
           m=m+1
           if(well2(8,m).gt.1e30) then
              do mm=m,ifrl(well2(7,m))
                 well2(18,mm)=IDwell
              enddo
              m=ifrl(well2(7,m))
           else
              well2(18,m)=IDwell
           endif
           IDwell=IDwell+1
        enddo
c
c   Echo input to iout file
c
      if (iwelpt.eq.0) then
        if (NQreject.gt.0) then
          txt = ' wells were outside of the model domain.'
          write (iout,'(1h0,5x,i5,a50)')NQreject, txt
          txt = 'The rejected pumpage totaled: '
          write (iout,'(1h0,a34,g14.5)') txt, Qreject
        endif
c
        write (iout,'(1h0,10x,i5,10h MNW WELLS)') nwell2
        write(iout,'(3x,4h No.,3x,3hLay,3x,3hRow,3x,3hCol,4x,6hStress,
     +        3x,8hQW param,6x,2hRw,7x,4hSkin,4x,8hWL Limit,
     +        4x,8hWL Refer,3x,12hNonLinear Cp,2x,8hQW Group,2x,
     +        12hCell-To-Well,2x,8hMin-Qoff,2x,7hMin-Qon,2x,
     +        16hSite Identifier  )')
c
        do m = 1, nwell2
          n = INT(well2(1,m))
          k = (n-1)/(ncol*nrow) + 1
          j = mod((n-1),ncol*nrow)/ncol + 1
          i = mod((n-1),ncol) + 1
          igrp = INT(well2(9,m))
c
          rw = well2(5,m)
          if( rw .lt. -zero ) then
            cond = -rw
          else
            Qact = well2(3,m)
            sk = well2(6,m)
            Cf = well2(16,m)
            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
            if( rw .lt. zero ) cond = cond * 1.0D3
          endif
          well2(11,m) = cond
c
c ---------Modified OUTPUT to hide internal pointers that "Look Funny" --KJH-- July 10, 2003
          if( well2(8,m) .gt. 1.0D30 )then
            if( well2(7,m) .lt. 1.0D30 )then
              ne = ifrl(well2(7,m))
              hlim = well2(7,ne)
              hrfw = well2(8,ne)
            else
            endif
          else
            hlim = well2(7,m)
            hrfw = well2(8,m)
          endif
          write (iout,'(1x,4i6,6(1x,g10.4),g13.6,i10,g13.6,
     +          2f10.3,2x,a32)')
     +          m, k,j,i, (well2(ii,m),ii=3,6),hlim, hrfw,
     +          well2(16,m), igrp, well2(11,m),
     +          (well2(ii,m)*100.0D0, ii = 13,14), MNWsite(m)
c
        enddo
      else
        write (iout,*) 'WELLS WILL NOT BE PRINTED'
      endif
c
c  Write blank fields in Auxillary BYNODE file if KPER=1 & IO>0
c
      if (totim.lt.1e-26 .and. iowell2(2).ne.0 ) then
        io = abs(iowell2(2))
        do m = 1, nwell2
          n = ifrl( well2(1,m) )
          write (abs(iowell2(2)),'(a32,1x,2i8)')MNWsite(m),m, n
        enddo
      endif
c
c  Write blank fields in Auxillary QSUM file if KPER=1 & IO>0
c
      if (totim.lt.1e-26 .and. iowell2(3).ne.0 ) then
        io = abs(iowell2(3))
        m = 0
        do while( m .lt. nwell2 )
          m = m + 1
          if( well2(8,m) .gt. 1.0D30 ) then
            ne  = ifrl( well2(7,m) )
            write (abs(iowell2(3)),'(a32,1x,i5.5,1h-,i5.5)')
     +             MNWsite(m),m,ne
            m = ne
          endif
        enddo
      endif
c
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF1MNW1ad(nwell2,mxwel2,well2,ibound,delr,delc,cr,cc,
     +               hy,small,Hdry,hnew, ncol, nrow, nodes,LAYHDT,BOTM,
     &                NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,
     &                HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     Update Qact for wells that were constrained
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      COMMON /BCFCOM/LAYCON(999)
      common /rev23/ PLOSS, iwelpt
      INTEGER IGRP, IUHUF, IULPF, IUBCF, NWELL2, MXWEL2, NODES, IBOUND,
     &        NCOL, NROW, LAYCON, NBOTM, NLAY, LAYHDT, M, N, IFRL, NE,
     &        IIN, IWELPT
      REAL HDRY, DELR, DELC, CR, CC, HY, BOTM, HANI, HK, HKCC, TRPY
      DOUBLE PRECISION ZERO, PLOSS
      double precision well2
      double precision qres,rw,cond,Qact,sk,Cf,qoff,qon,Qsmall,qdes,
     * csum,chsum,hwell,hlim,href,ipole,ddmax,ddsim,qpot,ratio,small
      dimension well2(18,mxwel2), ibound(nodes)
      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
      dimension hy(nodes)
      dimension hnew(nodes)
      double precision hnew
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
     &          LAYHDT(NLAY), TRPY(NLAY)
      double precision cel2wel
C
      zero = 1.0D-8
c
c1------if number of wells <= 0 then return.
      if(nwell2.le.0) return
c
c   Compute cell-to-well conductance for each well node
c
      do m = 1, nwell2
        n = ifrl( well2(1,m) )
        qres = well2(15,m)
c-----if the cell is inactive or specified then bypass processing.
        if( ibound(n).ne.0 ) then
          rw = well2(5,m)
          if( rw .lt. -zero ) then
            cond = -rw
          else
            Qact = well2(3,m)
            sk = well2(6,m)
            Cf = well2(16,m)
            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
            if( rw .lt. zero ) cond = cond * 1.0D3
          endif
          well2(11,m) = cond
        endif
      enddo
c
c   Allow constrained wells a new chance with the next time step
c
      m = 0
      do while( m .lt. nwell2 )
        m = m + 1
        qoff = well2(13,m)
        qon  = well2(14,m)
        qact = well2(3,m)
        Qsmall = small
c
c   A very large # in WL reference array (8,m) triggers multi-node calculation
c
        if( well2(8,m) .gt. 1.0D30 ) then
c     Compute hwell / Qpot for multi-node well
          ne  = ifrl( well2(7,m) )
          qdes = well2(15,ne)
          csum = 0.000D0
          chsum = 0.000D0
          qact = 0.0000D0
          Qsmall = small*abs(qdes)
          do iin = m, ne
            n = ifrl( well2(1,iin) )
            if( ibound(n) .ne. 0 ) then
              csum  = csum  + well2(11,iin)
              chsum = chsum + well2(11,iin)*hnew(n)
              qact  = qact  + well2( 3,iin)
            else
              qact  = 0.0000D0
            endif
          enddo
c---div0 ---  CSUM could go to zero if the entire well is dry
          if( csum .gt. zero ) then
            hwell = ( qdes + chsum ) / csum
          else
            hwell = hnew(n)
          endif
          m = ne
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
          ipole = 0
          if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
          hlim = well2(7,ne)
          href = well2(8,ne)
          ddmax = ipole*( hlim - href )
          ddsim = ipole*( hwell - href )
          qpot = hlim*csum - chsum
          if( ddsim .gt. ddmax ) then
            hwell = hlim
            qpot = hwell*csum - chsum
          endif
          cond = csum
        else       !  End of multi-node conditioning IF statement
c     Compute hwell / Qpot for single-node well
          n = ifrl( well2(1,m) )
          cond = well2(11,m)
          hlim = well2(7,m)
          qpot = ( hlim - hnew(n) )*cond
          qdes = well2(15,m)
        endif
c
c  Compute ratio of potential/desired flow rates
        ratio = 1.00D0
        if( abs(qdes) .gt. small ) ratio =  qpot / qdes
        if( ratio .gt. 0.9999D0 ) then
          ratio =  1.000D0
          Qpot = Qdes
        endif
c  Check if potential flow rate is below cutoff
        if( ratio .lt. Qoff ) then
          Qact = 0.000D0
          Qdes = Qact
          well2(2,m) = Qdes
          well2(3,m) = Qact
c  Check if potential flow rate is above restart threshold
        elseif( ratio.gt.Qon .and. abs(qact).lt.Qsmall ) then
          Qdes = well2(15,m)
          well2(2,m) = Qdes
          well2(3,m) = Qpot
        else
c  Otherwise leave the flow rate alone
        endif
c
      enddo   ! End of overall test loop
c
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF1MNW1fm(nwell2,mxwel2,well2,ibound,delr,delc,cr,cc,
     +    hy,small,Hdry, hcof, rhs, hnew, ncol, nrow, nodes,kiter,
     +    NoMoIter,LAYHDT,BOTM,NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,
     &    TRPY,HKCC,HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     add well flow to source term
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      INTEGER MXWEL2, NODES, IBOUND, NCOL, NROW, NBOTM, NLAY, IUHUF,
     &        IULPF, IUBCF, NOMOITER, KITER, NWELL2, LAYCON, LAYHDT,
     &        M, N, IFRL, IWELPT, NE, IIN, IQSLV
      REAL DELR, DELC, CR, CC, HY, HCOF, RHS, HANI, HK, HKCC, HDRY,
     &     BOTM, TRPY
      DOUBLE PRECISION PLOSS
      double precision well2
      double precision zero,qres,rw,cond,Qact,sk,Cf,qdes,csum,chsum,
     * hwell,ipole,hlim,href,ddmax,ddsim,ratio,dhc2w,small
      dimension well2(18,mxwel2), ibound(nodes)
      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
      dimension hy(nodes)
      dimension hcof(nodes), rhs(nodes)
      dimension hnew(nodes)
      double precision hnew
      double precision cel2wel
      COMMON /BCFCOM/LAYCON(999)
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
     &          LAYHDT(NLAY), TRPY(NLAY)
C
      zero = 1.0D-20
c
c                 CR( i, j, k)    ------>   CR  i + 1/2
c                 CC( i, j, k)    ------>   CC  j + 1/2
c                 CV( i, j, k)    ------>   CV  k + 1/2
c
c1------if number of wells <= 0 then return.
      if(nwell2.le.0) return
c
c   Compute cell-to-well conductance for each well node
c
      do m = 1, nwell2
        n = ifrl( well2(1,m) )
        qres = well2(15,m)
c-----if the cell is inactive or specified then bypass processing.
        if( ibound(n).ne.0 ) then
          rw = well2(5,m)
          if( rw .lt. -zero ) then
            cond = -rw
          else
            Qact = well2(3,m)
            sk = well2(6,m)
            Cf = well2(16,m)
            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
            if( rw .lt. zero ) cond = cond * 1.0D3
          endif
          well2(11,m) = cond
        endif
      enddo
c
c   Prepare components and limits of a multi-node well
      m = 0
      do while( m .lt. nwell2 )
        m = m + 1
        well2(10,m) = 1.0D31
c
c   A very large # in WL reference array (8,m) triggers multi-node calculation
c
        if( well2(8,m) .gt. 1.0D30 ) then
          ne  = ifrl( well2(7,m) )
          qdes = well2(2,ne)
          qact = qdes
          csum = 0.000D0
          chsum = 0.000D0
          do iin = m, ne
            n = ifrl( well2(1,iin) )
            if( ibound(n) .ne. 0 ) then
              csum  = csum  + well2(11,iin)
              chsum = chsum + well2(11,iin)*hnew(n)
            else
              well2(3,iin) = 0.0D0
            endif
          enddo
c---div0 ---  CSUM could go to zero if the entire well is dry
          if( csum .gt. zero ) then
            hwell = ( qact + chsum ) / csum
          else
            hwell = hnew(n)
          endif
c
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
          ipole = 0
          if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
          hlim = well2(7,ne)
          href = well2(8,ne)
          ddmax = ipole*( hlim - href )
          ddsim = ipole*( hwell - href )
c
          if( ddsim .gt. ddmax ) then
            hwell = hlim
            qact = hwell*csum - chsum
c      DD constraints that stop production are not tested until after the 2nd iteration
            if( kiter .gt.2 ) then
              ratio = 1.00D0
              if( abs(qdes) .gt. small ) ratio =  qact / qdes
              if( ratio .lt. 0.00001D0 ) then
                qact  = 0.000D0
                if (csum .gt. 0.0D0) then
                  hwell = chsum / csum
                else
                  hwell = hnew(n)
                endif
              endif
            endif
          endif
c
c   Assign flow rates and water levels to individual nodes
          do iin = m, ne
            n = ifrl( well2(1,iin) )
            well2(10,iin) = hwell
            qact = ( hwell - hnew(n) ) * well2(11,iin)
            well2(3,iin) = qact
          enddo
          m = ne
        endif       !  End of multi-node conditioning IF statement
      enddo       ! End of overall multi-node test loop
c
c2------process each well in the well list.
      m = 0
      do while( m .lt. nwell2 )
        m = m + 1
        n = ifrl( well2(1,m) )
        qdes = well2(2,m)
c-----if the cell is inactive then bypass processing.
        if( ibound(n).gt.0 ) then
          qact = well2(3,m)
          cond = well2(11,m)
c
          hlim = well2(7,m)
          href = well2(8,m)
          if( well2(10,m).gt.1.0D30 .and. cond.gt.zero ) then
            dhc2w = Qact / cond
c   Process single-node wells
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
            ipole = 0
            if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
            hwell = hnew(n) + dhc2w
            well2(10,m) = hwell
            ddsim = ipole*( hwell - href )
            ddmax = ipole*( hlim - href ) - small
            ratio = 1.00D0
            if( abs(qdes) .gt. zero ) ratio =  qact / qdes
            if( abs(ratio).gt. 1.00D0 ) qact = qdes
            if( ratio     .lt. zero ) qact = 0.0D0
c    Well will be simulated as a specified rate or GHB
            iqslv = 0
            if( ddsim.gt.ddmax .and. ddmax.gt.zero ) iqslv = 1
            if((qdes-qact)**2 .gt. small           ) iqslv = 1
            if(abs(qact).lt.zero .and.  ddsim.gt.ddmax) iqslv = 0
            if(abs(qact).lt.zero .and.  ddsim.lt.ddmax) iqslv = 1
            if(abs(qdes).lt.zero .or. ratio.gt.1.0D0-zero ) iqslv = 0
c
          elseif( cond.lt.zero ) then
            qact = 0.0D0
            iqslv = 0
          else
c Process multi-node wells, Constraints were already tested when allocating flow
            if( mod(kiter,2).eq.0 .and. abs(qact).gt.small ) then
              hlim = well2(10,m)
              iqslv = 1
            else
              qact = well2(3,m)
              iqslv = 0
            endif
          endif
c
c   Modify HCOF and RHS arrays
          if( iqslv.ne.0 .and. kiter.gt.1 .and. kiter.lt.NoMoIter ) then
            qact = ( hlim - hnew(n) ) * cond
            hcof(n) = hcof(n) - cond
            rhs(n)  = rhs(n)  - cond * hlim
          else
c  Specify Q and solve for head;  add Q to RHS accumulator.
            rhs(n) = rhs(n) - qact
          endif
          well2(3,m) = qact
C--SEAWAT: ADDED FOLLOWING LINE SO MNW FLUX IS AVAILABLE TO MT3D
          well2(17,m) = qact
        endif
      enddo       !    End of DO WHILE loop
c
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF1MNW1bd(MNWsite,nwell2,mxwel2,vbnm,vbvl,msum,delt,
     +        well2,ibound,hnew,ncol,nrow,nodes,nstp,kstp,kper,iwl2cb,
     +             icbcfl,buff,iout,iowell2,totim,Hdry,PERTIM)
C     VERSION 20030710 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     calculate volumetric budget for wells
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      INTEGER MXWEL2, MSUM , NODES, IBOUND, IBD, NAUX, NLAY, N, M,
     &        IGRP1, M2, IGRP2, IMULT, IL,M IR, IC, NE, IOCH, IWELPT,
     &        IOBYND, IIN, IOQSUM, IOC, NWELVL, IOUT, ICBCFL, IWL2CB,
     &        KPER, KSTP, NSTP, NROW, NCOL, NWELL2, IOWELL2, IFRL, IR
      REAL HDRY, VBVL, BUFF, PERTIM, TOTIM, DELT, q2, WELL2SP
      DOUBLE PRECISION HWELL, PLOSS
      double precision well2
      double precision zero,ratin,ratout,qwsum,qsum,qwbar,DryTest,q,
     * qd,hlim,href,dd,s,ipole,sNL,sL,qin,qout,qwfsum
      dimension MNWsite(mxwel2)
      dimension vbvl(4,msum),well2(18,mxwel2),
     1          ibound(nodes), buff(nodes)
      dimension iowell2(3)
      dimension hnew(nodes)
      dimension well2sp(1)
      double precision hnew
c
      character*16 text,vbnm(msum),AUXTXT(5)
      character*32 MNWsite
c             ----+----1----+-
      text = '             MNW'
      zero = 1.D-25
c     ------------------------------------------------------------------
c
cljk moved this line from below
      nlay = nodes / ncol / nrow
c  clear ratin and ratout accumulators.
      ratin=0.D0
      ratout=0.D0
      ibd=0
      IF(IWL2CB.GT.0) IBD=ICBCFL
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX = 0   !!   Set to zero -- Change order to dump
c         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,auxtxt,IWL2CB,NCOL,NROW,NLAY,
     1          NWELL2,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
c  clear the buffer.
      do n = 1, nodes
        buff(n)=0.000000000
      enddo
c -----print the header for individual rates if requested(iwl2cb<0).
      if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) then
        write(iout,'(/,1x,a16,9h PERIOD =,i5,8h  STEP =,i5)')
     +              text, kper,kstp
        write(iout,900)
      endif
  900 format(1x,6h Entry,4h LAY,4h ROW,4h COL,
     + 9x,1hQ,6x,6hH-Well,7x,6hH-Cell,7x,6hDD    ,7x,6hQW-Avg,
     + 6x,8hs-LINEAR,3x,11hs-NonLINEAR)

c  Create WEL1 file if iowell2(1) > 0
      if( iowell2(1).gt.0 .and. nstp.eq.kstp )
     +       write(iowell2(1),'(1i10)') nwell2
c
c2------if there are no wells do not accumulate flow
      if(nwell2.gt.0) then
c
c     Compute flow weighted QW values and store in well2(11,m)
c
        do m = 1, nwell2
          well2(11,m) = well2(3,m) * well2(4,m)
          well2(12,m) = 0.D0
          if( well2(4,m).lt.0.D0 .or. well2(3,m).gt.0.D0 ) then
            well2(11,m) = -1.D0
            well2(12,m) =  1.D0
          endif
        enddo
c
        do m = 1, nwell2
          igrp1 = ifrl( well2(9,m) )
          if( well2(12,m) .lt. 0.5D0 ) then
            qwsum = 0.0000D0
            qsum = 0.0000D0
            do m2 = m, nwell2
              igrp2 = ifrl( well2(9,m2) )
              if( igrp1.eq.igrp2 .and. well2(12,m2).lt.0.5D0) then
                qwsum = qwsum + well2(11,m2)
                qsum  = qsum  + well2(3,m2)
                well2(12,m2) = 1
              endif
            enddo
c
            qwbar = qwsum
            if( qsum**2.gt.zero ) qwbar = qwsum / qsum
            do m2 = m, nwell2
              igrp2 = ifrl( well2(9,m2) )
              if( igrp1.eq.igrp2 .and. well2(4,m2).ge.0.0D0 )
     +            well2(11,m2) = QWbar
            enddo
          endif
        enddo
c
        imult = 0
        do m = 1,nwell2
          n = ifrl( well2(1,m) )
          DryTest = Hnew(n) - Hdry
          if(ABS(DryTest).lt.zero) then
            well2(3,m) = 0.0D0
          endif
          q = well2(3,m)
          well2(17,m)=q     !!7/13/2003 - CZ: preserve q
c
c    Report all wells with production less than the desired rate......
          if(ibound(n).ne.0 .or. ABS(DryTest).lt.zero) then
            il = (n-1) / (ncol*nrow) + 1
            ir = mod((n-1),ncol*nrow)/ncol + 1
            ic = mod((n-1),ncol) + 1
            qd = well2(2,m)
            hlim = well2(7,m)
c -----Modified OUTPUT to hide internal pointers that "Look Funny" in DD column--KJH-- July 10, 2003
            if( well2(8,m) .gt. 1.0D30 )then
              imult = 1
              if( well2(7,m) .lt. 1.0D30 )then
                ne = ifrl(well2(7,m))
                href = well2(8,ne)
              else
              endif
            else
              href = well2(8,m)
            endif
            hwell = well2(10,m)
            QWbar = well2(11,m)
            dd  = hwell - href
c
            ioch = 0
            if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) ioch = 1
c -----print the individual rates if requested(iwl2cb<0).
            if( ioch.eq.1 ) then
              s   = hnew(n) - hwell
              IPOLE = 0
              if( abs(s).gt.zero )  ipole = s / abs(s)
              sNL = ipole * well2(16,m) * abs(q)**PLoss
              sL  = s - sNL
              write(iout,'(1x,i6,3i4,9(1x,g12.6))')
     +         m,il,ir,ic,q, hwell,hnew(n), dd, qwbar, sL, sNL
            endif
c
c -----print the individual rates to auxillary file if requested(iwl2cb<0).
            iobynd = abs(iowell2(2))
            if( iobynd.gt.0 ) then
              if(  ioch.eq.1 .or. iowell2(2).lt.0)then
                write(iobynd,'(a32,1x,2i8,6(1x,g14.8))')
     +          MNWsite(m),m,n,totim,q, hwell,hnew(n), qwbar
              endif
            endif
c  Create WEL1 file if iowell2(1) > 0
            if( iowell2(1).gt.0 .and. nstp.eq.kstp ) then
              write(iowell2(1),'(i9,2i10,1x,g10.4,i10,2x,6(1x,g10.4))')
     +        il,ir,ic,q,0,qd, hwell, hnew(n), dd,href,qwbar
            endif
c
            buff(n) = buff(n) + q
            if( q.ge.0.0D0 ) then
c -----pumping rate is positive(recharge). add it to ratin.
              ratin = ratin + q
            else
c -----pumping rate is negative(discharge). add it to ratout.
              ratout = ratout - q
            endif
          endif
        enddo
c
c   Sum components of  multi-node wells
c
c -----print the header for multi-node rates if requested(iwl2cb<0).
        if(  ioch.eq.1  .and. imult.eq.1 ) then
          write(iout,'(/,5x,31h Multi-Node Rates & Average QW )')
          write(iout,901)
  901     format(1x,16hSite Identifier ,5x,18hENTRY: Begin - End,
     +     2x,7hQ-Total,7x,6hH-Well,7x,6hDD    ,7x,6hQW-Avg)
        endif
c
        m = 0
        do while( m .lt. nwell2 )
          m = m + 1
          if( well2(8,m) .gt. 1.0D30 ) then
            ne  = ifrl( well2(7,m) )
            qwsum = 0.000D0
            qwfsum = 0.000D0
            qsum = 0.000D0
            qin  = 0.000D0
            qout = 0.000D0
            do iin = m, ne
              n = ifrl( well2(1,iin) )
              if( ibound(n).eq.0 ) well2(3,iin) = 0.0D0
              if( well2(4,iin).ge.0.0D0 .and. well2(3,iin).le.0.0D0 )
     &            then
                qwfsum  = qwfsum + well2(3,iin)
                qwsum   = qwsum  + well2(3,iin)*well2(4,iin)
              endif
              if( well2(3,iin).le.0.0D0 ) then
                qin = qin  + well2(3,iin)
              else
                qout = qout  + well2(3,iin)
              endif
              qsum  = qsum  + well2(3,iin)
              well2(3,iin) = 0.00000D0
            enddo
            well2(3,ne) = qsum
c -----print the summed rates if requested(iwl2cb<0).
            qwbar = well2(4,ne)
            if(qwfsum**2 .gt. zero ) qwbar = qwsum / qwfsum
            href = well2(8,ne)
            hwell = well2(10,ne)
            dd  = hwell - href
            if(  ioch.eq.1  ) then
              write(iout,'(A26,1x,2i6,6(1x,g12.6))')
     +         MNWsite(m),m,ne,qsum, hwell, dd, qwbar
            endif
c -----print the summed rates to auxillary file if requested .
            ioQsum = abs(iowell2(3))
            if( ioQsum.gt.0 ) then
              if(  ioch.eq.1 .or. iowell2(3).lt.0)then
                write(ioQsum,102)
     +          MNWsite(m),m,ne,totim,qin,qout, qsum, hwell, qwbar
  102           format(A32,1x,i5.5,1h-,i5.5,12(1x,g14.8))
              endif
            endif
            m = ne
          endif
        enddo
        if(  ioch.eq.1  .and. imult.eq.1 ) then
          write(iout,*)
        endif
c
c  ----- END  MULTI-NODE reporting section -------------
c
cljk        nlay = nodes / ncol / nrow
c6------if cell-by-cell flows will be saved call ubudsv to record them
        if( abs(iwl2cb).gt.0 .and. icbcfl.ne.0 ) then           !! BooBoo Fix--July 10,2003  KJH
          ioc = abs(iwl2cb)
          if( ibd.eq.2 ) then   !!  Write COMPACT budget
            NWELVL  = 1  !!  Dummy value
            do m = 1, nwell2
              n = ifrl( well2(1,m) )
              q = well2(3,m)
              q2=well2(17,m)
cljk          call UBDSVB(ioc,ncol, nrow,n,1,1,Q,well2(1,m),
Cerb 8/24/07  call UBDSVB(ioc,ncol, nrow,n,1,1,Q2,well2(1,m),
Cerb 8/24/07  UBDSVB requires REAL arguments; defined WELL2SP so that it gets
Cerb          promoted w/o loss of precision when using DP compiler option
              WELL2SP(1) = well2(1,m)  
              call UBDSVB(ioc,ncol, nrow,n,1,1,Q2,WELL2SP,
     +                    NWELVL,NAUX,5,IBOUND,NLAY)
            enddo
          else                  !!  Write full 3D array
            call ubudsv(kstp,kper,text,ioc, buff,ncol,nrow,nlay,iout)
          endif
        endif
      endif
c
c7------move rates into vbvl for printing by module bas1ot.
      vbvl(3,msum)=ratin
      vbvl(4,msum)=ratout
c
c8------move rates times time step length into vbvl accumulators.
      vbvl(1,msum) = vbvl(1,msum) + ratin*delt
      vbvl(2,msum) = vbvl(2,msum) + ratout*delt
c
c9------move budget term labels into vbnm for printing.
      vbnm(msum) = text
c
c10-----increment budget term counter(msum).
      msum = msum + 1
c
c11-----return
      return
      end
c
c_________________________________________________________________________________
c
      subroutine GWF1MNW1OT(MNWsite,well2,nwell2,mxwel2,iowell2,OUTname)
C     VERSION 20020819 KJH
c
c     ******************************************************************
c    Sort well output into useful tables
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NWELL2, MXWEL2, IOWELL2, IOSTART, I, IOBYND, IOQSUM, ICNT,
     &        ISTOP, NODE, ME, IOT, IFRL, IOPT, NE, NB, IO, K1, K2
      REAL HCELL, TIMMULT
      DOUBLE PRECISION QWBAR, QSUM
      double precision well2
      double precision zero,Tnow,hwell,conc,Qt,Qin,Qout,Q,TimeIN
      double precision hwellOUT,concOUT
      dimension MNWsite(mxwel2)
      dimension well2(18,mxwel2)
      dimension iowell2(3)
      character*1  tab
      character*32  MNWsite, TempTag,TT, LastTag, EOFtag
      character*200 OUTname
c
      tab = char(9)
      zero = 1.D-25
      IOstart = 1000
      EOFtag = 'EndOfFile__EndOfFile__EndOfFile_'
c   Set Flag for printing header info once
      do i = 1, mxwel2
        well2(16,i)= 0     !! 16 = Header Flag
      enddo
c
c   Site file names are constructed to be OUTname_MNWsite.txt
c   All info for a well are dumped as tab delimited output
c
c------------------------------------------------------------------
c     The 16 rows of the well array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = Well node locator
c         2   = Net Discharge
c         3   = Water level in wellbore
c         4   = Water Quality
c         5   = Qin  ---------------MNW ONLY--------
c         6   = Qout
c         7   = Q-node   / Net Discharge
c         8   = MN-Flag --- Number of nodes / well
c         9   = I/O unit for well output
c        16   = Header Flag   Print= 0 / NoPrint = 1
c------------------------------------------------------------------
c
c   Test auxillary output files for cleaning data sets
      iobynd = abs(iowell2(2))
      if( iobynd.gt.0 ) then
        write(iobynd,'(a32)') EOFtag
        rewind(iobynd)
        read(iobynd,'(a)')
      else
        return
      endif
c
      ioQsum = abs(iowell2(3))
      if( ioQsum.gt.0 ) then
        write(ioQsum,'(a32)') EOFtag
        rewind(ioQsum)
        read(ioQsum,'(a)')
      else
        return
      endif
c
      nwell2= 0
      iCNT  = 0
      iSTOP = 0
      Tnow  = -1.0000D0
      LastTag = 'NO-PRINT'
c
      do while( iSTOP.eq.0 )
        read(iobynd,'(a32,1x,2i8,6g15.8)')
     +     TempTag,mE,node,TimeIN,Q, hwell,hcell, Conc
c
c   Test for output before accumulating INFO
        if(LastTag(1:8).ne.'NO-PRINT' .and. TempTag.ne.LastTag )then  !! Output
          IOT = ifrl(well2(9,1))
c   Write a Header ?????
          ioPT = IOT - IOstart
          if( ifrl(well2(16,ioPT)).eq.0 ) then
            well2(16,ioPT) = 1
            if( iCNT.gt.1 ) then
              write(IOT,
     +        '(a4,a1,a9,a1,a6,a1,a13,a1,a10,a1,a11,99(a1,a4,i7.7))')
     +        'TIME',tab,'Discharge',tab,'H-Well',tab,'Concentration',
     +         tab,'Net-Inflow',tab,'Net-Outflow',
     +         (tab,'Node',ifrl(well2(1,i)), i=1,ICNT)
            else
              write(IOT,'(a4,a1,a9,a1,a6,a1,a13)')
     +        'TIME',tab,'Discharge',tab,'H-Well',tab,'Concentration'
            endif
          endif
c   END of "Write a Header"  Section
C _________________          Bug FIX 1/14/2006 KJH _______________________________________
c    Overwrote Hwell & Conc varibles before storing.
          HwellOUT = well2(3,iCNT)    !! Could use well2() array directly.  Opted for
          ConcOUT  = well2(4,iCNT)    !! copying to temporary variables for clarity.
c
c  Bug identified by Jacob Gudbjerg, DHI Water & Environment, DK-2970  Hørsholm
C___________________________________________________________________________________
          if( iCNT.gt.1 ) then
            Qt   = well2(7,1)
            Qin  = well2(5,1)
            Qout = well2(6,1)
            write(IOT,'(99(g15.8,a1))')
     +      well2(10,1),tab, Qt,tab, HwellOUT,tab, concOUT,
     +      tab,Qin, tab,Qout, (tab,well2(2,i),i=1,iCNT)
          else
            Qt = well2(2,1)
            write(IOT,'(99(g15.8,a1))')
     +      well2(10,1),tab, Qt,tab, HwellOUT,tab, concOUT
c
          endif
          iCNT = 0     !! RESET node counter
        endif

c   Is this the EOF?
        if( TempTag .eq. EOFtag ) iSTOP = 1
c
c   Skip if this is a NO-PRINT node
        if( TempTag(1:8).eq.'NO-PRINT' .or. TempTag.eq.EOFtag )then
          iCNT = 0
        else
          iCNT = iCNT + 1
c   Identify pointer
          call ioWellOUT(TempTag,OUTname,nwell2,
     +           mxwel2,MNWsite,IOstart,io)
          well2(1,iCNT) = node       !!  1 = Well node locator
          well2(2,iCNT) = Q          !!  2 = Net Discharge
          well2(3,iCNT) = Hwell      !!  3 = Water level in wellbore
          well2(4,iCNT) = Conc       !!  4 = Water Quality
          well2(8,1) = iCNT       !!  8 = MN-Flag --- Number of nodes / well
          well2(9,1) = io+IOstart !!  9 = IO output
          well2(10,1)= TimeIN     !! 10 = Time
c
c     Read MN well output if available
          if( TempTag.eq.LastTag )then    !! MN well
            if( iCNT.eq.2 .and. ioQsum.gt.0 ) then
              read (ioQsum,102)
     +        TT, nb,ne, TimMULT,qin,qout, qsum, Hwell, qwbar
  102         format(A32,1x,i5.5,1x,i5.5,12(1x,g14.8))
              well2(4,1) = QWbar       !!  4 = Water Quality
              well2(5,1) = Qin        !!  5 = Qin  ---------------MNW ONLY--------
              well2(6,1) = Qout       !!  6 = Qout
              well2(7,1) = Qsum       !!  7 = Qnet
            endif
          endif
        endif
c
c   Save Value of TEMPTAG for comparison
        LastTag = TempTag
      enddo
c
c   Add IO close routine here if needed
c
      return
      end
c
c_________________________________________________________________________________
c
      subroutine ioWellOUT(TempTag,OUTname,nwell2,
     +           mxwel2,MNWsite,IOstart,io)
      IMPLICIT NONE
      INTEGER IO, IOSTART, NWELL2, MXWEL2, I, K1, K2
      dimension MNWsite(mxwel2)
      character*32  MNWsite, TempTag
      character*200 OUTname
      character*256 txt
c
      i = 0
      io = 0
      do while( i.lt.nwell2 )
        i = i + 1
        if( MNWsite(i).eq.TempTag ) then
          io = i
          i = nwell2
        endif
      enddo
c
      if( io.eq.0 ) then
        nwell2 = nwell2 + 1
        MNWsite(nwell2) = TempTag
        io = nwell2
c   Build output file name and Open file
        k1 = index(OUTname,' ') - 1
        k2 = index(TempTag,' ') - 1
        if( k2.eq.0 .and. TempTag(32:32).ne.' ' ) k2 = 32
        txt = OUTname(1:k1)//'.'//TempTag(1:k2)//'.txt'
        open(io+IOstart,file=txt)
      endif
c
      return
      end
c
c_________________________________________________________________________________
c
      DOUBLE PRECISION function cel2wel(delr,delc,cr,cc,hy,hnew,
     +                 ncol,nrow,nodes,n,rw,skin,Q,Cf,PLoss,small,Hdry,
     &                 LAYHDT,BOTM,NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,TRPY,
     &                 HKCC,HANI)
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
c     ******************************************************************
C     Note: BCF, when LAYCON=0 or 2, does not save cell-by-cell
C     Transmissivity (T) values.  Instead, it converts the cell-by-cell
C     T values to branch conductances CR and CC, using harmonic
C     averaging.  When BCF is used, the method used in this routine to
C     generate cell-specific values of Tx and Ty is an approximation
C     based on CR and CC.  When LPF or HUF is used, cell-by-cell
C     hydraulic-conductivity values are stored, this approximation is
C     not needed, and the values generated for Tx and Ty are exact --
C     ERB 1/29/01.
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCOL, NROW, NODES, IUHUF, IULPF, IUBCF, N, NBOTM,
     &        NLAY, LAYHDT, LAYCBD, LBOTM, LAYCON, LAYWET, LAYVKA,
     &        LAYAVG, LAYTYP, IX, IY, IZ
      REAL DELR, DELC, CR, CC, HY, HDRY, BOTM, HANI, HK, HKCC, TRPY,
     &     CHANI
      DOUBLE PRECISION PLOSS
      double precision rw,Q,Cf,zero,pi,skin,small,ro,AH,KY,TempKX,
     * dx,dy,top,bot,dxp,dxm,Txm,Txp,dyp,dym,Typ,Tym,div,Txx,Tyy,
     * THICK,upper,yx4,xy4,Tpi2,A,B,C
      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
      dimension hy(nodes)
      dimension hnew(nodes)
      double precision hnew
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
     &          LAYHDT(NLAY), TRPY(NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /BCFCOM/LAYCON(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
C     ------------------------------------------------------------------
 1000 FORMAT(/1X,
     &'***ERROR: MNW1 PACKAGE DOES NOT SUPPORT HEAD-DEPENDENT',/,
     &' THICKNESS OPTION OF SELECTED FLOW PACKAGE',/,
     &' (MNW1 DOES FULLY SUPPORT BCF, LPF, AND HUF PACKAGES)',/,
     &' -- STOP EXECUTION (CEL2WEL)')
C
      pi = 3.1415926535897932D0
      zero = 1.D-25
C
      ix   = mod((n-1),ncol)+1
      dx   = delr(ix)
      iy   = mod((n-1),ncol*nrow)/ncol + 1
      dy   = delc(iy)
      iz   = int((n-1)/(ncol*nrow)) + 1
      top = BOTM(IX,IY,LBOTM(IZ)-1)
      bot = BOTM(IX,IY,LBOTM(IZ))
C
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
      AH = 1.0D0
      IF (IULPF.GT.0) THEN
        IF (CHANI(IZ).GT.0.0) THEN
          AH = CHANI(IZ)
        ELSE
          AH = HANI(IX,IY,IZ)
        ENDIF
      ELSEIF (IUHUF.GT.0) THEN
        TempKX = HK(N)
        KY = HKCC(IX,IY,IZ)
        AH = KY/TempKX
      ELSEIF (IUBCF.GT.0) THEN
        AH = TRPY(IZ)
      ENDIF
C
      if (LAYHDT(IZ).EQ.0) then
C       THICKNESS IS NOT HEAD-DEPENDENT
        IF (IULPF.EQ.0 .AND. IUHUF.EQ.0) THEN
C         BCF OR ANOTHER FLOW PACKAGE, OTHER THAN LPF OR HUF, IS ACTIVE
          dxp  = dx
          Txp  = 0.00000D0
          if( ix .lt. ncol ) then
            dxp = delr(ix+1)
            Txp  = cr(n) * (dx+dxp) / 2.D0
          endif
          dxm = dx
          Txm  = Txp
          if( ix .gt. 1  ) then
            dxm = delr(ix-1)
            Txm  = cr(n-1) * (dx+dxm) / 2.D0
          endif
          if( Txp.lt.small ) Txp = Txm
          if( Txm.lt.small ) Txm = Txp
c
          dyp  = dy
          Typ  = 0.00000D0
          if( iy .lt. nrow ) then
            dyp = delc(iy+1)
            Typ  = cc(n) * (dy+dyp) / 2.D0
          endif
          dym = dy
          Tym  = Typ
          if( iy .gt. 1 ) then
            dym = delc(iy-1)
            Tym  = cc(n-ncol) * (dy+dym) / 2.D0
          endif
          if( Typ.lt.small ) Typ = Tym
          if( Tym.lt.small ) Tym = Typ
          Txp = Txp / dy
          Txm = Txm / dy
          Typ = Typ / dx
          Tym = Tym / dx
c
c  Eliminate zero values .....
c
          if( Typ.lt.small .or. nrow.lt.2 )  then
            Typ = Txp
            Tym = Txm
          endif
c
          if( Txp.lt.small .or. ncol.lt.2 )  then
            Txp = Typ
            Txm = Tym
          endif
c
c   Assuming expansion of grid is slight, if present, & that Txx and Tyy of the adjacent
c  cells are about the same value.
          Txx = 0.00000000D0
          div  = Txp + Txm
          if( div.gt.small ) Txx  = 2*Txp*Txm / div
          Tyy = 0.00000000D0
          div  = Typ + Tym
          if( div.gt.small ) Tyy  = 2*Typ*Tym / div
          if( Txx.gt.small .and. Tyy.lt.small ) Tyy = Txx
          if( Tyy.gt.small .and. Txx.lt.small ) Txx = Tyy
        ELSE
C         LPF OR HUF IS ACTIVE
          THICK = TOP-BOT
          TXX = HK(N)*THICK
          TYY = TXX*AH
        ENDIF
      else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
        upper = hnew(n)
        IF (IUBCF.GT.0) THEN
          if (LAYCON(IZ).EQ.3) then
            if( upper.gt.top ) upper = top
          endif
          TempKX = hy(n)        !!  BCF Hydraulic Conductivity array
        ELSEIF (IULPF.GT.0 .OR. IUHUF.GT.0) THEN
          if( upper.gt.top ) upper = top
          TempKX = hk(n)        !!  LPF Hydraulic Conductivity array
        ENDIF
        thick = upper - bot
c   set thickness / conductance to 0 if cell is dry
        if( (hnew(n)-Hdry )**2 .lt. zero ) thick = 0.000000000000D0
        Txx = TempKX * thick
        if( Txx .lt.zero ) Txx = 0.000000000000000D0
        Tyy = Txx * AH
      endif
c
      if( rw.lt.zero .or. Txx.lt.zero .or. Tyy.lt.zero ) then
        cel2wel = ( Txx * Tyy )** 0.5D0
      else
        yx4 = (Tyy/Txx)**0.25D0
        xy4 = (Txx/Tyy)**0.25D0
        ro = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
c
        Tpi2 = 2.D0*pi * (Txx*Tyy)**0.5D0
        A = log(ro/rw) / Tpi2
        if( Ploss .gt. 0.99D0 ) then
          B = Skin
          C = Cf * abs(Q)**(PLoss-1)
        else
          B = Skin / Tpi2
          C = 0.000000000D0
        endif
        cel2wel = A + B + C
        cel2wel = 1.000000D0 / cel2wel
      endif
c
      return
      end
c
c_________________________________________________________________________________
c
      integer function idirect(n1, n2, ncol,nrow)
c
c     ******************************************************************
c     Define direction of pointer along a row, column, or layer
c     ******************************************************************
c
      IMPLICIT NONE
      INTEGER NROW, NCOL, N2, N1
      idirect = ncol
      if( abs( n2-n1 ) .gt. ncol*nrow ) idirect = ncol*nrow
      if( abs( n2-n1 ) .lt. ncol      ) idirect = 1
      if( n2 .lt. n1 ) idirect = -idirect
c
      return
      end
c
c_________________________________________________________________________________
c
c
      integer function itxend( txt )
      IMPLICIT NONE
      INTEGER K
      character*256 txt
      k = 256
      do while( txt(k:k).eq.' ' .and. k.gt.1 )
        k = k - 1
      enddo
      itxend = k
c
      return
      end
c
c_________________________________________________________________________________
c
      integer function ifrl( r )
      IMPLICIT NONE
      INTEGER IP
      DOUBLE PRECISION R
      ip = abs(r) + 0.5D0
      if( r .lt. 0.000D0 )  ip = -ip
      ifrl = ip
      return
      end
c
c_________________________________________________________________________________
c
c   NCREAD: reads lines of input and ignores lines that begin with a "#" sign.
c          All information after a ! is wiped from the input card.
      subroutine ncread(io,txt,ierr)
      IMPLICIT NONE
      INTEGER IERR, IO, IOALT, IOFLIP, KI, IOHOLD
      character*128  afile
      character*256  txt,tx2
      data ioflip,ioalt /69,69/
c
      ierr = 0
    5 read(io,'(a)',end=10)  txt
      if( txt(1:1) .eq. '#' )  goto 5
c
      ki = index(txt,'!')
      if( ki.gt.0 ) then
        txt(ki:256) = '                                                '
      endif
c
      tx2 = txt
      call UPCASE(tx2)
c
c    Test for switching control to an auxillary input file
c
      ki = index(txt,':')
      if( index(tx2,'REDIRECT').gt.0 .and. ki.gt.0 ) then
        afile = txt(ki+1:256)
        ki = index(afile,'  ') - 1
        iohold = io
        io = ioflip
        ioflip = iohold
        open(io,file=afile(1:ki),status='OLD',err=20)
        goto 5
      endif
c
c    Test for returning io control from auxillary input to master input file
c
      if( index(tx2,'RETURN')  .gt.0 .and.
     +    index(tx2,'CONTROL') .gt.0      ) goto 10
c
      ki = index(tx2,'<END>')
      if( ki .gt. 0 ) then
        ierr = 1
        txt(ki+5:256) = '                                           '
      endif
c
      if( index(tx2,'<STOP>') .gt. 0 ) ierr = 2
      return
c
c    Report error in opening auxillary input file and stop
c
   20 write(*,*)
      write(*,*) '  ERROR opening auxillary input file  '
      write(*,*)
      write(*,'(2x,10h The file:,2x,a40,16h does not exist.)')  afile
      write(*,*)
C
C     When compiling MNW with Modflow-96, comment out the call to
C     USTOP and uncomment the STOP statement
      CALL USTOP(' ')
C      STOP
C
   10 txt(1:3) = 'EOF'
      if( io .eq. ioalt ) then
        close(io)
        iohold = io
        io = ioflip
        ioflip = iohold
        goto 5
      else
        ierr = -1
      endif
c
      return
      end
c
c_________________________________________________________________________________
c
      subroutine shorten(txt,test,n,tx2)
      IMPLICIT NONE
      INTEGER N, KI, KC
      character*256 txt,tx2,test
c
      tx2 = '???????'
      ki = index(txt,test(1:n))
      if(ki.gt.0) then
        tx2 = txt(ki:256)
        kc = index(tx2,':')+1
        tx2 = tx2(kc:256)
        kc = index(tx2,':')+1
        if( kc .gt. 1 ) tx2 = tx2(1:kc-1)
      endif
      return
      end
c
c_________________________________________________________________________________
c  GENERIC UTILITIES that were IN MNWutil.for  file
c_________________________________________________________________________________
c
      subroutine conden(txt,n)
      IMPLICIT NONE
      INTEGER I, L, K, M, N
      character*256 txt
      if(n.gt.64) n=64
      do  i=1,256-n
        txt(n+i:n+i)=' '
      enddo
c
      l=0
      do 10 k=1,n*2
      l=l+1
      if(txt(l:l).eq.' ')then
      do m=l,n
        txt(m:m)=txt(m+1:m+1)
      enddo
      l=l-1
      endif
   10 continue
      return
      end
c
c_________________________________________________________________________________
c
      subroutine bakped(n,io)
      IMPLICIT NONE
      INTEGER IO, N, I
c
      do i=1,n
        backspace(io)
      enddo
c
      return
      end
c
c_________________________________________________________________________________
c
      subroutine qread(r,ni,ain,ierr)
      IMPLICIT NONE
      INTEGER IERR, NI, MRNV, I, N, KI, ND, ISTAT
      parameter (mrnv=25)
      double precision r
      dimension r(mrnv)
      character*1   tab
      character*16  form
      character*256 a256,ain
      tab = char(9)           ! sets tab delimiter
c
c   r(ni+1) records the number of non-numeric entries that were attempted to be read as a number
c   r(ni+2) records the last column that was read from the card
c
      r(ni+1) = -1.0000D0
      a256 = ain
      do i = 1, 256
        if( a256(i:i).eq.tab ) a256(i:i) = ' '
        if( a256(i:i).eq.',' ) a256(i:i) = ' '
        if( a256(i:i).eq.':' ) a256(i:i) = ' '
        if( a256(i:i).eq.'=' ) a256(i:i) = ' '
      enddo
      n = 1
      i = 0
   11 r(ni+1) = r(ni+1) + 1.D0
   10 i = i+1
      if( i.ge.256) goto 15
      if( a256(i:i).eq.' ' ) then
        a256(i:i) = '?'
        goto 10
      endif
c
      ki = index(a256,' ')-1
      nd = ki - i + 1
      form ='(F??.0)              '
      write(form(3:4),'(i2.2)') nd
CERB  Fix for bug that caused i to be incremented by only 1 position
CERB  each time the read statement returns an error.  This bug also
CERB  incremented r(ni+1) unnecessarily.  With Lahey-compiled code, the
CERB  buggy version would read a final E in a word (without returning an
CERB  error) as a zero.
CERB      read (a256(i:ki),form,err=11,end=10) r(n)
      READ (A256(I:KI),FORM,ERR=13,IOSTAT=ISTAT) R(N)
   13 CONTINUE
      i = ki
      IF (ISTAT.GT.0) GOTO 11  ! PART OF BUG FIX -- ERB
      n = n+1
      if( n.le.ni .and. i.lt.256) goto 10
c
  15  n  = n-1
      ierr = ni - n
      r(ni+2) = i
      return
      end

