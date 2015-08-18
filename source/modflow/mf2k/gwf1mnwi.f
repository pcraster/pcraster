c   GZH  20090405 
C
C
C GWF1MNWIAL READ INIT DATA AND ALLOCATE SPACE FOR MNW WELLS DESIGNATED FOR OBSERVATION
C
C     ******************************************************************
C
      SUBROUTINE GWF1MNWIAL(INMNWI,INMNW2,IOUT,LCMNIO,
     & Wel1flag,QSUMflag,BYNDflag,ISUMZ,MNWOBS)    
C
C     ******************************************************************
      IMPLICIT NONE
      INTEGER INMNWI,INMNW2,IOUT,LCMNIO,Wel1flag,QSUMflag,BYNDflag,
     & ISUMZ,MNWOBS
C
      IF(INMNWI.GT.0.AND.INMNW2.LE.0) THEN
        WRITE(IOUT,*) '***ERROR*** : MNWI PACKAGE CAN ONLY BE 
     *USED IF MNW2 PACKAGE IS ACTIVE'
        CALL USTOP('MNWI ERROR')
      END IF  
C
      IF(INMNWI.EQ.0) THEN
        LCMNIO=1
      ELSE
c     if transport on, read concflag
        READ(INMNWI,*) Wel1flag,QSUMflag,BYNDflag
        WRITE(IOUT,*) 'MNWI Package input:'
        WRITE(IOUT,*) 'Wel1flag = ',Wel1flag
        WRITE(IOUT,*) 'QSUMflag = ',QSUMflag
        WRITE(IOUT,*) 'BYNDflag = ',BYNDflag
        WRITE(IOUT,*) 
C
        READ(INMNWI,*) MNWOBS
        IF(MNWOBS.LT.0) THEN
          WRITE(IOUT,*) 'MNWOBS MUST BE > 0'
          CALL USTOP(' ')
        END IF
C
        LCMNIO=ISUMZ
        ISUMZ=ISUMZ+MNWOBS*6+1
      WRITE(IOUT,103) MNWOBS*6
  103 FORMAT(1X,I8,' ELEMENTS IN RZ ARRAY ARE USED BY MNWI')
      END IF
C
      RETURN
      END
c
c_________________________________________________________________________________
c
C
C  GWF1MNWIRP READ INPUT FILE FOR MNW2 WELLS DESIGNATED FOR OBSERVATION 
C
C     ******************************************************************
C
      SUBROUTINE GWF1MNWIRP(MNWOBS,IOUT,MNWILST,INMNWI,MNWIID,MNWMAX,
     & MNW2,WELLID,GWTUNIT,NMNWVL)
C
C     ******************************************************************
C
C     READ LOCATIONS OF MNW2 WELLS DESIGNATED FOR OBSERVATION 
C     ******************************************************************
C
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MNWOBS,IOUT,IOB,IS_SITE,INMNWI,iw,MNWMAX,GWTUNIT,NMNWVL
      DOUBLE PRECISION MNWILST,MNW2
      CHARACTER*20 WELLID,SITE,MNWIID,MSITE
      DIMENSION mnw2(NMNWVL,mNWMAX),WELLID(MNWMAX),
     & MNWIID(MNWMAX),MNWILST(6,MNWOBS)
C
C     ******************************************************************
C
      IF(MNWOBS.EQ.0) THEN
       RETURN
      ENDIF
      IF(MNWOBS.EQ.1) THEN
       WRITE (IOUT,120) MNWOBS
      ELSEIF(MNWOBS.GT.1) THEN
       WRITE (IOUT,140) MNWOBS
      ELSEIF(MNWOBS.LT.1) THEN
       RETURN
      END IF
      WRITE (IOUT,150)
      IF(MNWOBS.GT.MNWMAX) then
        write(iout,*) '***ERROR*** MNWOBS > MNWMAX'
        CALL USTOP('MNWI ERROR')
      end if
C
C  Initialize data array
      MNWILST=0.0
C READ THE FIRST RECORD
      IOB=1
      IS_SITE=0
c
c MNWILST(1,IOB) is Well # in MNW list
c MNWILST(2,IOB) is net volume in/out well
c MNWILST(3,IOB) is unit number for output
c MNWILST(4,IOB4) is QNDflag
c MNWILST(5,IOB) is QBHflag
c MNWILST(6,IOB) is CONCflag
c
      if(GWTUNIT.GT.0) then
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB),MNWILST(6,IOB)
      else
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       MNWILST(6,IOB)=0
      end if
      SITE=MNWIID(IOB)
      call UPCASE(SITE)
c check site vs list of site names in MNWSITE
c Loop over all MNW locations
c   Loop over all wells
      do iw=1,MNWMAX
        MSITE=WELLID(iw)
        call UPCASE(MSITE)
        IF(SITE.EQ.MSITE) THEN
          IS_SITE=1
          MNWILST(1,IOB)=iw
        END IF
      end do      
C
      WRITE(IOUT,'(I8,3X,A12,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
     &  INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB))       
      IF(IS_SITE.EQ.0) THEN
         WRITE(IOUT,*) '***ERROR***   SITE FOR MNWI ',
     *'WELL DESIGNATED FOR OBSERVATION NOT FOUND'
         CALL USTOP('MNWI ERROR')
      ENDIF
C CYCLE THROUGH THE REMAINING RECORDS
      DO IOB=2,MNWOBS
        IS_SITE=0
      if(GWTUNIT.GT.0) then
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB),MNWILST(6,IOB)
      else
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       MNWILST(6,IOB)=0
      end if
c check site vs list of site names in WELLID
        SITE=MNWIID(IOB)
        call UPCASE(SITE)
c   Loop over all wells
        do iw=1,MNWMAX
          MSITE=WELLID(iw)
          call UPCASE(MSITE)
          IF(SITE.EQ.MSITE) THEN
            IS_SITE=1
            MNWILST(1,IOB)=iw
          END IF
        end do      
C
        WRITE(IOUT,'(I8,3X,A12,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
     &  INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB))       
        IF(IS_SITE.EQ.0) THEN
         WRITE(IOUT,*) '***ERROR***   SITE FOR MNWI ',
     *'WELL DESIGNATED FOR OBSERVATION NOT FOUND'
          CALL USTOP('MNWI ERROR')
        ENDIF
C
      END DO
            WRITE(IOUT,'(140A)') 'DATA FOR MNW WELLS DESIGNATED FOR
     * OBSERVATION WILL BE WRITTEN ON UNIT NUMBERS LISTED ABOVE' 
            WRITE(IOUT,'(/)')
  120 FORMAT(///'SITE ID FOR',I4,
     * ' MNW2 WELL DESIGNATED FOR OBSERVATION:')
  140 FORMAT(///'SITE IDS FOR',I4,
     * ' MNW2 WELLS DESIGNATED FOR OBSERVATION:')
  150 FORMAT(/'  WELL #   SITE ID         UNIT  QNDflag QBHflag')
      RETURN
      END       
C
c_________________________________________________________________________________
c
      subroutine GWF1MNWIOT(Wel1flag,QSUMflag,BYNDflag,nstp,kstp,
     & nmnw2,MNWMAX,MNW2,MNWNOD,NODTOT,WELLID,TOTIM,hnew,ncol,nrow,nlay,
     & MNWOBS,DELT,MNWIID,MNWILST,kper,ntotnod,iout,hdry,NMNWVL)
C     VERSION 20090405 GZH
c
c     ******************************************************************
c    Sort well output into useful tables
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      IMPLICIT NONE
      ALLOCATABLE QBH(:)
      INTEGER Wel1flag,QSUMflag,BYNDflag,nstp,kstp,nmnw2,
     & MNWMAX,NODTOT,iw,INODE,firstnode,lastnode,il,ir,ic,ioutqsum,
     & ncol,nrow,nlay,MNWOBS,iwobs,QNDflag,QBHflag,QCONCflag,i,istat,
     & iout,kper,ibh,ind,NNODES,numvals,ntotnod,nd,NMNWVL,IAUX,
     & NAUX
      REAL TOTIM,DELT,hdry
      DOUBLE PRECISION mnw2,MNWNOD,q,hwell,qin,qout,qnet,hnew,hcell,
     & MNWILST,QBH,small
      COMMON /MNW2COM/MNWAUX(5)
      CHARACTER*20 WELLID,obssite,MNWIID
      CHARACTER*50 LFRMAT
      CHARACTER*16 MNWAUX
      DIMENSION mnw2(NMNWVL,mNWMAX),MNWNOD(31,NODTOT),WELLID(MNWMAX),
     & hnew(ncol,nrow,nlay),MNWIID(MNWMAX),MNWILST(6,MNWOBS)
c
c------------------------------------------------------------------
      small=1D-5
C     
      ALLOCATE(QBH(NODTOT),STAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        WRITE(*,1700)ISTAT
 1700   FORMAT(1X,'ALLOCATION OF MNW ROUTING ARRAY FAILED,',
     &  ' RETURNED ERROR MESSAGE NUMBER: ',I6)
        CALL USTOP(' ')
      ENDIF
C
c Print WEL1 file
      if(Wel1flag.gt.0) then
c print max number of wells, set IWELCB=0
C-LFK     CHECK IF AUXILIARY VARIABLES PRESENT, & WRITE THEM IF PRESENT
       NAUX=NMNWVL-30
       IF (NAUX.LE.0) THEN
        if(kper.eq.1.and.kstp.eq.1)
     &  write(Wel1flag,'(2i10)') NODTOT,0
       ELSE
        if(kper.eq.1.and.kstp.eq.1)
     &  write(Wel1flag,1750) NODTOT,0,(mnwaux(iaux),iaux=1,naux)
 1750 FORMAT(2I10,2x,5(:(' AUX ',16A,2X)))
       END IF
c only write at end of stress period (nstp=kstp)
        if(nstp.eq.kstp) then
c write number of wells
          write(Wel1flag,'(i10)') ntotnod 
c   Loop over all wells
          do iw=1,MNWMAX
c   Loop over nodes in well
              firstnode=MNW2(4,iw)
              lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
              do INODE=firstnode,lastnode
                il=MNWNOD(1,INODE)              
                ir=MNWNOD(2,INODE)              
                ic=MNWNOD(3,INODE)              
                q=MNWNOD(4,INODE)
c   Write L,R,C,q (& OPTIONAL AUX values)
               IF (NAUX.LE.0) THEN
                write(Wel1flag,'(i9,2i10,1x,g15.6)') il,ir,ic,q       
               ELSE
        write(Wel1flag,1760)il,ir,ic,q,(MNW2(30+IAUX,IW),IAUX=1,NAUX)  
 1760 FORMAT (i9,2i10,1x,g15.6,2X,5(:(f4.0,4x)))
               END IF
              end do
          end do
        end if
      end if
c  Print QSUM file
      if(QSUMflag.gt.0) then
c   Write header
        if(kstp.eq.1.and.kper.eq.1) then
           write(QSUMflag,'(200A)') 'WELLID                    Totim    
     &        Qin           Qout           Qnet          hwell'
        end if
c   Loop over all wells
        do iw=1,MNWMAX
          qin=0.0D0
          qout=0.0D0
          qnet=0.0D0
c   Only operate on active wells (MNW2(1,iw)=1)
          if (MNW2(1,iw).EQ.1) then
c   Loop over nodes in well
            firstnode=MNW2(4,iw)
            lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
            hwell=MNW2(17,iw)
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              q=MNWNOD(4,INODE)
              if(q.lt.0.0D0) then
                qin=qin+q
              else
                qout=qout+q
              end if
              qnet=qnet+q
            end do
c            if(qnet.lt.(small*qin)) qnet=0.d0
            write(QSUMflag,'(A20,5(1x,1Pg14.6))')
     &              WELLID(iw),totim,qin,qout,qnet,hwell
          end if
        end do     
      end if
c   Print BYND (ByNode) file
      if(BYNDflag.gt.0) then
c   Write header
        if(kstp.eq.1.and.kper.eq.1) then
           write(BYNDflag,'(101A)') 'WELLID                NODE   Lay ',
     &'  Row   Col        Totim        Q-node         hwell         hcel
     &l   Seepage elev.'
        end if
c   Loop over all wells
        do iw=1,MNWMAX
c   Only operate on active wells (MNW2(1,iw)=1)
          if (MNW2(1,iw).EQ.1) then
c   Loop over nodes in well
            firstnode=MNW2(4,iw)
            lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
            hwell=MNW2(17,iw)
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              q=MNWNOD(4,INODE)
              hcell=hnew(ic,ir,il)
              nd=INODE-firstnode+1
c   If no seepage face in cell, don't print seepage elev.
              if(MNWNOD(15,INODE).EQ.hwell.or.
     &           MNWNOD(15,INODE).eq.Hdry.OR.MNW2(2,iw).eq.1) then
                write(BYNDflag,'(A20,4i6,1x,1P4e14.6)')
     &              WELLID(iw),nd,il,ir,ic,totim,q,hwell,hcell
              else
c   If seepage face in cell, MNWNOD(15) will hold the bottom elev of the cell,
c   which is used with hcell to get the gradient used to calculate the Q for the
c   seepage face.  
                write(BYNDflag,'(A20,4i6,1x,1P5e14.6)')
     &              WELLID(iw),nd,il,ir,ic,totim,q,hwell,hcell,
     &              MNWNOD(15,INODE)
              
              end if
            end do
         end if
        end do           
      end if
c
c  Print MNWOBS files
      do iwobs=1,MNWOBS    
        qnet=0.d0
        obssite=MNWIID(iwobs)
        iw=MNWILST(1,iwobs)  
c   Loop over nodes in well
          firstnode=MNW2(4,iw)
          lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
          hwell=MNW2(17,iw)
          qin=0.D0
          qout=0.D0
          do INODE=firstnode,lastnode
            q=MNWNOD(4,INODE)
            if(q.lt.0.0D0) then
              qin=qin+q
            else
              qout=qout+q
            end if
            qnet=qnet+q
          end do
c   Cumulative volume for this well
          MNWILST(2,iwobs)=MNWILST(2,iwobs)+qnet*DELT
            
c get NNODES
          NNODES=INT(ABS(MNW2(2,iw)))
c
c  Print according to flags
          QNDflag=INT(MNWILST(4,iwobs))
          QBHflag=INT(MNWILST(5,iwobs))
          QCONCflag=INT(MNWILST(6,iwobs))
c
          if(QBHflag.gt.0)  
     &      call GWF1MNW2BH(MNWMAX,mnw2,NODTOT,MNWNOD,iout,NMNWVL,iw)
c
C  Create format for header--single node
C  Ignore ND and BH flags
C  Create format for header--single node
          if(NNODES.eq.1) then
            if(kstp.eq.1.and.kper.eq.1) then
          Write (INT(MNWILST(3,iwobs)),'(A)') 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell'
            end if
            write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6)')
     &             WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell
          else
c  all multi-node well output below
          if(QNDflag.eq.0) then
            if(QBHflag.eq.0) then
              if(QCONCflag.eq.0) then            
c  QNDflag=0, QBHflag=0, QCONCflag=0
c write header
                if(kstp.eq.1.and.kper.eq.1) then
          Write (INT(MNWILST(3,iwobs)),'(120A)') 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet      Cum.Vol.         hwell
     &   '
                end if
c   Only operate on active wells (MNW2(1,iw)=1)
                if (MNW2(1,iw).EQ.1) then

                write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6)')
     &             WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell
                end if
              else
c  QNDflag=0, QBHflag=0, QCONCflag=1
c write header
                if(kstp.eq.1.and.kper.eq.1) then
          Write (INT(MNWILST(3,iwobs)),'(120A)') 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     &   CONC'
                end if
                if (MNW2(1,iw).EQ.1) then
                 write(INT(MNWILST(3,iwobs)),'(A20,1x,1P6e14.6,3A)')
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &  'N/A'
c
                end if
              end if
            else
              if(QCONCflag.eq.0) then            
c  QNDflag=0, QBHflag=1, QCONCflag=0
c write "smart" header
                if(kstp.eq.1.and.kper.eq.1) then
C  Create format for header  
            WRITE(LFRMAT,14) NNODES-1
  14  FORMAT('(A,',I4,'I12)')
C  WRITE FORMAT FOR HEADER LINE
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & QBH_seg-->1',(ibh,ibh=2,NNODES)
                end if
                if (MNW2(1,iw).EQ.1) then

C Create format for write
                WRITE (LFRMAT,355) NNODES
  355           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')
             write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(27,i),i=firstnode,lastnode)
                end if
              else
c  QNDflag=0, QBHflag=1, QCONCflag=1
c write header
                if(kstp.eq.1.and.kper.eq.1) then
C  Create format for header  
            WRITE(LFRMAT,15) NNODES-1
  15  FORMAT('(A,',I4,'I12,A)')
C  WRITE FORMAT FOR HEADER LINE
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & QBH_seg-->1',(ibh,ibh=2,NNODES),'     CONC'
                end if
                if (MNW2(1,iw).EQ.1) then
C Create format for write
                WRITE (LFRMAT,255) NNODES
  255           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))',3A)
                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(27,i),i=firstnode,lastnode),'N/A'
                end if
              end if
            end if  
          else
            if(QBHflag.eq.0) then
              if(QCONCflag.eq.0) then            
c  QNDflag=1, QBHflag=0, QCONCflag=0
c write "smart" header
                if(kstp.eq.1.and.kper.eq.1) then
C  Create format for header  
            WRITE(LFRMAT,14) NNODES-1
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES)
                end if
                if (MNW2(1,iw).EQ.1) then
C Create format for write
                WRITE (LFRMAT,155) NNODES
  155           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')

                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode)
                end if
              else
c  QNDflag=1, QBHflag=0, QCONCflag=1
c
c write "smart" header
                if(kstp.eq.1.and.kper.eq.1) then
C  Create format for header  
            WRITE(LFRMAT,15) NNODES-1
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),'   CONC'
                end if
                if (MNW2(1,iw).EQ.1) then
C Create format for write
                WRITE (LFRMAT,455) NNODES
  455           FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4)),3A')

                write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode),'N/A'
c
                end if
              end if
            else
              if(QCONCflag.eq.0) then            
c  QNDflag=1, QBHflag=1, QCONCflag=0
C Create format for write

c write "smart" header
                if(kstp.eq.1.and.kper.eq.1) then
C  Create format for header  
          WRITE(LFRMAT,16) NNODES-1,NNODES-1
  16  FORMAT('(A,',I4,'I12,A,',I4,'I12)')
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),
     &' QBH_seg-->1',(ibh,ibh=2,NNODES)
                end if
                if (MNW2(1,iw).EQ.1) then
       numvals=NNODES+NNODES
               WRITE (LFRMAT,156) numvals
  156          FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))')
                  write(INT(MNWILST(3,iwobs)),LFRMAT)
     &              WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),
     &              hwell,(MNWNOD(4,i),i=firstnode,lastnode),
     &              (MNWNOD(27,i),i=firstnode,lastnode)

                end if
              else
c  QNDflag=1, QBHflag=1, QCONCflag=1
       if(kstp.eq.1.and.kper.eq.1) then
          WRITE(LFRMAT,17) NNODES-1,NNODES-1
  17  FORMAT('(A,',I4,'I12,A,',I4,'I12,A)')
          Write (INT(MNWILST(3,iwobs)),LFRMAT) 'WELLID                
     &       TOTIM           Qin
     &          Qout          Qnet         QCumu         hwell
     & Flow@Nd-->1',(ind,ind=2,NNODES),
     &' QBH_seg-->1',(ibh,ibh=2,NNODES),'   CONC'
        end if
                if (MNW2(1,iw).EQ.1) then
                WRITE (LFRMAT,456) NNODES, 
     & NNODES
  456          FORMAT ('(A20,1p6e14.6,',I4,'(1pE12.4))',I4,'(1pE12.4))')
                  write(INT(MNWILST(3,iwobs)),LFRMAT)
     &            WELLID(iw),totim,qin,qout,qnet,MNWILST(2,IWOBS),hwell,
     &              (MNWNOD(4,i),i=firstnode,lastnode),
     &              (MNWNOD(27,i),i=firstnode,lastnode),'N/A'
                end if
              end if
            end if  
          end if  
          end if   
      end do
c
      return
      end
