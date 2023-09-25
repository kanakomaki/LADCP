      program ladcp1_n
****************************************************
*LADCP Processing PROGRAMS (shear integration methods with bottom track vel)
*1. ladcp1_n.f
*2. ladcp2_n.f
*3. ladcp3_n.f 
*4. ladcp4_n.f 
*5. ladcp5_n.f
*6. ladcp6_n.f
*6. ladcp7_n.f
*6. ladcp8_n.f
*
*input     LADCP DATA,  CTD DATA (if you have)
*
*input directory     ../DAT/    
*output directory    ../ANA/LADCP1/, ../ANA/ECHO/ 
*
*last revised on Oct/8/2011
****************************************************
* ---------------------------------------------------------------
* CTD data format (time[day],depth[m])
* time format
*      2010June15,12:00:00 =  15+(12*3600s)/86400s = 15.5[day] 
* WinADCP output variables 
*      Ens,YR,MO,DA,HH,MM,SS,HH,,Pit,Rol,Hea,Tem,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,BTE,BTN,BTV,BTe,"   ""BD1""   ""BD2""   ""BD3""   ""BD4"""
* ---------------------------------------------------------------
      implicit  none
      character ctdfile*7,ladcpfile*7,st*3
      integer   nbin,mode
      real      dmax,bin1,depcell
      real*8    tsa,tbtm1,tbtm2
*
*SET Time difference ( CTD time - LADCP time ) = tsa[day]
      tsa = 0.0    ![day]
*SET MODE
      mode  = 1     ! 1:down&up, 2:only down, 3: only up
*-----------------------
*SET station name
      st         ='004'
*SET CTD maximun depth & CTD time at bottom
c      dmax  = 1068.        ![m]   maximum depth of ctd data
c      tbtm1 = 14.3323842         ![day] reach the bottom
c      tbtm2 = 14.3323852        ![day] leave the bottom
c      dmax  = 1005.8        ![m]   maximum depth of ctd data
c      tbtm1 = 14.41725         ![day] reach the bottom
c      tbtm2 = 14.41825        ![day] leave the bottom
c      dmax  = 971.8        ![m]   maximum depth of ctd data
c      tbtm1 = 14.52287         ![day] reach the bottom
c      tbtm2 = 14.52490        ![day] leave the bottom
      dmax  = 1017.        ![m]   maximum depth of ctd data
      tbtm1 = 14.59640     ![day] reach the bottom
      tbtm2 = 14.59732        ![day] leave the bottom
*SET open file name
      ctdfile    =''//st//'.ctd'  !CTD data
      ladcpfile  =''//st//'.asc'  !WinADCP output (station name.asc)
      nbin       = 26           !LADCP data maximum bin number 0~40
*-----------------------
*LADCP DATA
      depcell =  8.0  ![m],  downward [>0]
      bin1    = 10.23 ![m]
C
c      CALL LTIME(st,ctdfile,ladcpfile,tsa,nbin,bin1,depcell,tbtm1, !USE CTD DATA
c     &           tbtm2,mode,dmax)
      CALL LTIME2(st,ctdfile,ladcpfile,tsa,nbin,bin1,depcell,tbtm1,
     &           tbtm2,mode,dmax)
C
*REMOVE SEAFLOOR REFLECTION
      CALL EI(dmax,mode,st)
C
*FILE ECHO INTENSITY DATA
      CALL EIFILE(mode,st)
C
      STOP
      END
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE RDDEPTH(time,xdep0,ctdfile)
      REAL*8 time
      character ctdfile*7
      open(18,file='../DAT/'//ctdfile//'',form='formatted',status='old')
      xdep0=9999.
      write(*,*)ctdfile,time
C
      read(18,*)
      do i=1,1000000
      read(18,*,end=100)xt,xpres,xdep !data of 2011/9/14
      xt=14. + (xt/86400.)            !data of 2011/9/14
c      write(*,*)xt,'CTD',xt,xpres,xdep
c
c
      if(abs(time-xt).LT.0.00001)then
         xdep0=xdep
         goto 100
      end if
      end do
c
 100  continue
      close(18)
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * * * *

*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE LTIME(st,ctdfile,ladcpfile,tsa,nbin,bin1,depcell,
     &           tbtm1,tbtm2,mode,dmax)
      DIMENSION da(300),xu(40),xv(40),xw(40),xev(40),xco(40),
     &xei(40),xd(40)
      REAL*8    time,tsa,tbtm1,tbtm2
      CHARACTER ctdfile*7, ladcpfile*7, st*3
      jmax = 13 + (nbin*7) + 8
C
      open(10,file=
     &'../DAT/'//ladcpfile//'',
     &           form='formatted',status='old')
      open(20,file=
     &'../ANA/LADCP1/'//st//'.btr',
     &           form='formatted')
      open(22,file=
     &'../ANA/LADCP1/'//st//'.vbt_dw',
     &           form='formatted')
      open(24,file=
     &'../ANA/LADCP1/'//st//'.vbt_up',
     &           form='formatted')
C
      if(mode.eq.1)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted')
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted')
      elseif(mode.eq.2)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted')
      else
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted')
      endif
C
 7    format(f7.1,x,f12.5,2x,f7.1,2x,f3.0,2x,9(f8.2,x))
 8    format(f7.1,x,f12.5,2x,f7.1,2x,f3.0,2x,10(f8.2,x))
C DATA Reading
      do n=1,16
         read(10,*)
      enddo
C
      do n=1,2000000
C
      do i=1,300
         da(i) = 9999.
      enddo
C
C
      read(10,*,end=200)(da(j),j=1,jmax)
      time= da(4)+(da(5)*3600.0+da(6)*60.0+da(7)+da(8)/100.0)/86400.0 !hh,mm,ss,hh
      time= time+tsa ![day]
      write(*,*)time
C
      CALL RDDEPTH(time,xdep0,ctdfile)
      if(xdep0.eq.9999.)then
         goto 199
      endif
C
      xpi = da(10)  !Pitch[deg]
      xro = da(11)  !Roll[deg]
      xhe = da(12)  !Heading[deg] (da(13)=temperature)
      xbu = da(13+7*nbin+1) !bottom track U [mm/s]
      xbv = da(13+7*nbin+2) !bottom track V [mm/s]
      xbw = da(13+7*nbin+3) !bottom track W [mm/s]
      xbev= da(13+7*nbin+4) !bottom track error velocity [cm/s]
      if(xbu.ne.9999.and.xbv.ne.9999..and.xbw.ne.9999.)then
         xbd=((da(13+7*nbin+4+1)+da(13+7*nbin+4+2)+da(13+7*nbin+4+3)+
     &      da(13+7*nbin+4+4) )/4.0)/100.    !bottom altitude[m]
      else
         xbd=9999.
      endif
      if(xbu.ne.9999)then
          write(*,*)da(1),xdep0,dmax,xbu,xbv,xbw,xbd
      endif
C 
C 
      do i=1,nbin
         xd(i)=bin1+(i-1)*depcell !bin depth
         xu(i)= da(13+2*nbin+i)
         xv(i)= da(13+3*nbin+i)
         xw(i)= da(13+4*nbin+i)
         xev(i)=da(13+5*nbin+i)
         xco(i)=da(13+1*nbin+i)
         xei(i)=da(13+i)          !echo intensity
           if(xbd.eq.9999.0)then
            xei(i)=da(13+i)
           elseif( (xdep0+xbd).lt.(xdep0+xd(i)) )then
            xei(i)=9999.0
           endif
      enddo
C
C Bottom-track velocity
      if( (abs(dmax-xdep0).LT.500.0).AND.
     &    (xdep0.LT.9999.0).AND.
     &    (xbu .LT.9999.0).AND.
     &    (xbv .LT.9999.0).AND.
     &    (xbw .LT.9999.0).AND.
     &    (xbev.LT.9999.0).AND.
     &    (abs(xpi).LT.20.0).AND.(abs(xro).LT.20.0)
     &)then
      write(20,8)da(1),time,xdep0,9.,xbu/10.,xbv/10.,xbw/10.,xbev/10.,
     &           9999.0,9999.0,xhe,xpi,xro,xbd
      end if
C
C Velocity data 
      if(time.lt.tbtm1)then
         nfile=21
      elseif(time.gt.tbtm2)then
         nfile=23
      else
         goto 9
      endif
C
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
C
C for bottom part only
 9    continue
      if(
     &(time.le.tbtm1.and.(tbtm1-time).lt.360./86400) )then ! 360s within the bottom
         nfile=22
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
      elseif(
     & (time.ge.tbtm2.and.(time-tbtm2).lt.360./86400) )then ! 360s within the bottom
         nfile=24
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
      endif
C
 199  continue
      end do
 200  continue
C
      close(21)
      close(20)
      close(22)
      close(23)
      close(10)
C
      RETURN
      END
**  *****  ***  ***  ***  **  ****   ****



*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE LTIME2(st,ctdfile,ladcpfile,tsa,nbin,bin1,depcell,
     &           tbtm1,tbtm2,mode,dmax)
      DIMENSION da(300),xu(40),xv(40),xw(40),xev(40),xco(40),xpg(40),
     &xei(40),xd(40)
      REAL*8    time,tsa,tbtm1,tbtm2,pretime
      CHARACTER ctdfile*7, ladcpfile*7, st*3
      jmax = 13 + (nbin*7) + 8
C
      open(10,file=
     &'../DAT/'//ladcpfile//'',
     &           form='formatted',status='old')
      open(20,file=
     &'../ANA/LADCP1/'//st//'.btr',
     &           form='formatted')
      open(22,file=
     &'../ANA/LADCP1/'//st//'.vbt_dw',
     &           form='formatted')
      open(24,file=
     &'../ANA/LADCP1/'//st//'.vbt_up',
     &           form='formatted')
C
      if(mode.eq.1)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted')
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted')
      elseif(mode.eq.2)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted')
      else
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted')
      endif
C
 7    format(f7.1,x,f12.5,2x,f7.1,2x,f3.0,2x,9(f8.2,x))
 8    format(f7.1,x,f12.5,2x,f7.1,2x,f3.0,2x,10(f8.2,x))
C DATA Reading
      do n=1,16
         read(10,*)
      enddo
C
      read(10,*,end=200)(da(j),j=1,jmax)
      time= da(4)+(da(5)*3600.0+da(6)*60.0+da(7)+da(8)/100.0)/86400.0 !hh,mm,ss,hh
      time= time+tsa ![day]
      pretime = time
      if(mode.eq.1.or.mode.eq.2)then
         prew = 0.0
         xdep0 = 0.0
      else
         prew = 0.0
         xdep0 = dmax
      endif
C     
      do n=1,20000000
C
      do i=1,300
         da(i) = 9999.
      enddo
C
C
      read(10,*,end=200)(da(j),j=1,jmax)
      time= da(4)+(da(5)*3600.0+da(6)*60.0+da(7)+da(8)/100.0)/86400.0 !hh,mm,ss,hh
      time= time+tsa ![day]
      write(*,*)time
C
      xpi = da(10)  !Pitch[deg]
      xro = da(11)  !Roll[deg]
      xhe = da(12)  !Heading[deg] (da(13)=temperature)
      xbu = da(13+7*nbin+1) !bottom track U [mm/s]
      xbv = da(13+7*nbin+2) !bottom track V [mm/s]
      xbw = da(13+7*nbin+3) !bottom track W [mm/s]
      xbev= da(13+7*nbin+4) !bottom track error velocity [cm/s]
      if(xbu.ne.9999.and.xbv.ne.9999..and.xbw.ne.9999.)then
         xbd=((da(13+7*nbin+4+1)+da(13+7*nbin+4+2)+da(13+7*nbin+4+3)+
     &      da(13+7*nbin+4+4) )/4.0)/100.    !bottom altitude[m]
      else
         xbd=9999.
      endif
C 
C 
      do i=1,nbin
         xd(i)=bin1+(i-1)*depcell !bin depth
         xu(i)= da(13+2*nbin+i)
         xv(i)= da(13+3*nbin+i)
         xw(i)= da(13+4*nbin+i)
         xev(i)=da(13+5*nbin+i)
         xco(i)=da(13+1*nbin+i)
         xpg(i)=da(13+6*nbin+i)
         xei(i)=da(13+i)          !echo intensity
           if(xbd.eq.9999.0)then
            xei(i)=da(13+i)
           elseif( (xdep0+xbd).lt.(xdep0+xd(i)) )then
            xei(i)=9999.0
           endif
      enddo
C
C depth cal 
C
      do i=1,nbin
      if( (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0)
     &)then
         sumw = sumw + (xw(i)/1000.)
         kow  = kow  + 1.
      endif
      enddo
      if(kow.gt.0)then
         xdep0 = xdep0 + (sumw/kow)*(time-pretime)*86400.
         prew  = (sumw/kow)
      else
         xdep0 = xdep0 + prew*(time-pretime)*86400.
      endif
       sumw =0.0
       kow = 0
       write(*,*)'depth=',xdep0
C
C
C Bottom-track velocity
      if( (abs(dmax-xdep0).LT.500.0).AND.
     &    (xdep0.LT.9999.0).AND.
     &    (xbu .LT.9999.0).AND.
     &    (xbv .LT.9999.0).AND.
     &    (xbw .LT.9999.0).AND.
     &    (xbev.LT.9999.0).AND.
     &    (abs(xpi).LT.20.0).AND.(abs(xro).LT.20.0)
     &)then
      write(20,8)da(1),time,xdep0,9.,xbu/10.,xbv/10.,xbw/10.,xbev/10.,
     &           9999.0,9999.0,xhe,xpi,xro,xbd
      end if
C
C Velocity data 
      if(time.lt.tbtm1)then
         nfile=21
      elseif(time.gt.tbtm2)then
         nfile=23
      else
         goto 9
      endif
C
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.60.0).AND.
     &    (xpg(i).GT.80.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
C
C for bottom part only
 9    continue
      if(
     &(time.le.tbtm1.and.(tbtm1-time).lt.360./86400) )then ! 360s within the bottom
         nfile=22
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
      elseif(
     & (time.ge.tbtm2.and.(time-tbtm2).lt.360./86400) )then ! 360s within the bottom
         nfile=24
      do i=1,nbin
      if( (xdep0.LT.9999.0).AND.
     &    (xu(i).LT.9999.0).AND.
     &    (xv(i).LT.9999.0).AND.
     &    (xw(i).LT.9999.0).AND.
     &    (xev(i).LT.9999.0).AND.
     &    (xco(i).GT.0.0).AND.
     &    (abs(xpi).LT.18.0).AND.(abs(xro).LT.18.0)
     &)then
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
      endif
C
 199  continue
      
      pretime = time
      end do
 200  continue
C
      close(21)
      close(20)
      close(22)
      close(23)
      close(10)
      close(24)
C
      RETURN
      END
**  *****  ***  ***  ***  **  ****   ****

*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE EI(dmax,mode,st)
      character st*3
*REMOVE REFLECTION AT THE BOTTOM
      open(22,file=
     &'../ANA/LADCP1/'//st//'.vbt',
     &           form='formatted',status='old')
      open(32,file=
     &'../ANA/LADCP1/'//st//'.ebt',
     &           form='formatted')
      na=22
      nb=32
      CALL FINDEI(dmax,na,nb)


      if(mode.eq.1)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted',status='old')
      open(31,file=
     &'../ANA/LADCP1/'//st//'.edw',
     &           form='formatted')
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted',status='old')
      open(33,file=
     &'../ANA/LADCP1/'//st//'.eup',
     &           form='formatted')
      na=21
      nb=31
      CALL FINDEI(dmax,na,nb)
      na=22
      nb=32
      CALL FINDEI(dmax,na,nb)
      na=23
      nb=33
      CALL FINDEI(dmax,na,nb)

      elseif(mode.eq.2)then
      open(21,file=
     &'../ANA/LADCP1/'//st//'.vdw',
     &           form='formatted',status='old')
      open(31,file=
     &'../ANA/LADCP1/'//st//'.edw',
     &           form='formatted')
      na=21
      nb=31
      CALL FINDEI(dmax,na,nb)

      else
      open(23,file=
     &'../ANA/LADCP1/'//st//'.vup',
     &           form='formatted',status='old')
      open(33,file=
     &'../ANA/LADCP1/'//st//'.eup',
     &           form='formatted')
      na=23
      nb=33
      CALL FINDEI(dmax,na,nb)

      endif

      close(21)
      close(22)
      close(23)
      close(31)
      close(32)
      close(33)
      RETURN
      END
*** *******  *****  *******  *****  ***** ****  *****
*----------------------------------------
      subroutine findei(dmax,na,nb)
      dimension x(40,11),x0(11),xt(10000),xn(10000)
      real*8 xt,xt0

      read(na,*,end=100)xn0,xt0,(x0(j),j=1,11)
      n=1
      k=1
      xt(n)=xt0
      xn(n)=xn0
      do j=1,11
         x(k,j)=x0(j)
      enddo

      do i=1,1000000
      read(na,*,end=100)xn0,xt0,(x0(j),j=1,11)

      if(xn0.ne.xn(n).and.abs(x(1,1)-dmax).lt.200.0)then
         CALL FINDMAX(x,k,max)
         do k=1,max
            write(nb,'(f7.1,f12.5,2x,11f8.2)')xn(n),xt(n),
     &      (x(k,j),j=1,11)
         enddo
         n=n+1
         k=1
         xt(n)=xt0
         xn(n)=xn0
        do j=1,11
            x(k,j)=x0(j)
         enddo
      elseif(xn0.ne.xn(n).and.abs(x(1,1)-dmax).GE.200.0)then
         do m=1,k
            write(nb,'(f7.1,f12.5,2x,11f8.2)')xn(n),xt(n),
     &            (x(m,j),j=1,11)
         enddo
         n=n+1
         k=1
         xt(n)=xt0
         xn(n)=xn0
         do j=1,11
            x(k,j)=x0(j)
         enddo
      elseif(xn0.eq.xn(n))then
         k=k+1
         do j=1,11
            x(k,j)=x0(j)
         enddo
      endif
      enddo
 100  continue
      close(na)

      write(*,*)'NMAX= ',n, ' NA,NB=',na,nb, ' DMAX=',dmax
      return
      end
*----------------------------------------
      subroutine FINDMAX(x,kmax,max)
      dimension x(40,11)

      do k=2,kmax
C         write(*,*)x(k,7),x(k-1,7)
         if(x(k,7).gt.x(k-1,7))then
            max=k-1
            goto 500
         else
            max=k
         endif
      enddo
      xbin=x(max,2)
      
 500  continue
C      write(*,*)'Bottom BIN=',x(max,2),max
c
      return
      end
*----------------------------------------
*   *   *   *   *   *   *   *   *   *   *   *  *   *   *   *    
      SUBROUTINE EIFILE(mode,st)
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   
      DIMENSION sum2(1000),sum3(1000),nko2(1000)
     &,nko3(1000),d(1000)
      CHARACTER updw*2,st*3
*Grid number
      nxmax=1000
*SET Bin number
      b0=3.0
*SET UPDW
      if(mode.eq.3)then
         updw = 'up'
      else
         updw = 'dw'
      endif
*
      do nx=1,nxmax
         d(nx)=(nx-1)*10.0
         sum2(nx)=0.0
         nko2(nx)=0.0
         sum3(nx)=0.0
         nko3(nx)=0.0
      end do
      dmax=0.0   
*READING
      open(10,file=
     &     '../ANA/LADCP1/'//st//'.e'//updw//'',
     &     form='formatted',status='old')
      open(20,file=
     &     '../ANA/ECHO/'//st//'echo.'//updw//'',form='formatted')
*
      do n=1,1000000
      read(10,*,end=100)xn,xt,xd,xi1,xu,xv,xw,xev,xei,xc,xhe,xpi,xro
         do nx=1,nxmax
            if((xi1.EQ.b0).AND.(xd.GT.d(nx)).AND.(xd.LE.d(nx+1)))then
               sum3(nx)=sum3(nx)+xei
               nko3(nx)=nko3(nx)+1
            end if
         end do
         
         if(xd.GT.dmax)then
            dmax=xd
         end if
         
      end do
 100  continue
C
      do nx=1,nxmax
      if((d(nx).LT.dmax).AND.(nko3(nx).NE.0))then
       write(20,'(f7.1,2x,f7.2,(2x,i5))')d(nx),
     & sum3(nx)/nko3(nx),nko3(nx)
      else if(d(nx).GT.dmax)then
       goto 200
      elseif((nko3(nx).EQ.0))then
       write(20,'(f7.1,2x,f7.2,(2x,i5))')d(nx),9999.0,nko3(nx)
      end if
 200  end do

      close(10)
      close(20)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   
