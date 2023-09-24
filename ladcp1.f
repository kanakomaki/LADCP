      program ladcp1
****************************************************
*PROGRAMS
*1. ladcp1.f
*2. ladcp2.f
*3. ladcp3.f 
*4. ladcpnaiso.f 
*5. ladcpashear5m.f
*6. ladcpvel_for_fitting.f
*
*input     LADCP DATA, CTD DATA (TIME,DEPTH)
*input     ./DAT/001.asc, ./DAT/ctd.asc
*output    ./DAT/LADCP1/001.vdw,./DAT/LADCP1/001.vbt,./DAT/LADCP1/001.vup
*output    ./DAT/LADCP1/001.edw,./DAT/LADCP1/001.ebt,./DAT/LADCP1/001.eup    
****************************************************
      implicit  none
      character ctdfile*7,ladcpfile*7
      integer   nbin
      real      dmax,bin1,depcell
      real*8    tsa,tbtm1,tbtm2
*
*SET Time difference ( CTD time - LADCP time ) = tsa[day]
      tsa = 0.0    ![day]
*SET CTD maximun depth & CTD time at bottom
      dmax  = 2498.0        ![m]   maximum depth of ctd data
      tbtm1 = 15.350978     ![day] reach the bottom
      tbtm2 = 15.352379     ![day] leave the bottom
*SET open file name
      ctdfile    ='ctd.asc'  !CTD data
      ladcpfile  ='001.asc'  !WinADCP output (station name.asc)
      nbin       = 25        !LADCP data maximum bin number 0~40
* ---------------------------------------------------------------
* CTD data format (time[day],depth[m])
* time format
*      2010June15,12:00:00 =  15+(12*3600s)/86700s = 15.5[day] 
* WinADCP output variables 
*      Ens,YR,MO,DA,HH,MM,SS,HH,,Pit,Rol,Hea,Tem,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,EAA,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,C5,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Eas,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Nor,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Ver,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,Err,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,PG4,BTE,BTN,BTV,BTe,"   ""BD1""   ""BD2""   ""BD3""   ""BD4"""
* ---------------------------------------------------------------
C
C
*LADCP DATA
      depcell =8.0 ![m],  downward [>0]
      bin1    =8.26 ![m]
      CALL LTIME(ctdfile,ladcpfile,tsa,nbin,bin1,depcell,tbtm1,tbtm2)
C
*REMOVE SEAFLOOR REFLECTION
      CALL EI(dmax)
C
*FILE ECHO INTENSITY DATA
C      CALL EIFILE
C
C
      STOP
      END
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE RDDEPTH(time,xdep0)
      REAL*8 time
c
      open(18,file='./DAT/ctd.asc',
     &form='formatted',status='old')
c
      xdep0=9999.
      do i=1,1000000
      read(18,*,end=100)xt,xdep
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
      SUBROUTINE LTIME(ctdfile,ladcpfile,tsa,nbin,bin1,depcell,
     &           tbtm1,tbtm2)
      DIMENSION da(300),xu(40),xv(40),xw(40),xev(40),xco(40),
     &xei(40),xd(40)
      REAL*8    time,tsa,tbtm1,tbtm2
      CHARACTER ctdfile*7, ladcpfile*7
C
      open(10,file=
     &'./DAT/001.asc',
     &           form='formatted',status='old')
      open(20,file=
     &'./DAT/LADCP1/001.btr',
     &           form='formatted')
      open(21,file=
     &'./DAT/LADCP1/001.vdw',
     &           form='formatted')
      open(22,file=
     &'./DAT/LADCP1/001.vbt',
     &           form='formatted')
      open(23,file=
     &'./DAT/LADCP1/001.vup',
     &           form='formatted')
C
 7    format(f6.1,x,f12.5,2x,f7.1,2x,f3.0,2x,9(f8.2,x))
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
      read(10,*,end=200)(da(j),j=1,196)
      time= da(4)+(da(5)*3600.0+da(6)*60.0+da(7)+da(8)/100.0)/86400.0 !hh,mm,ss,hh
      time= time+tsa ![day]
      CALL RDDEPTH(time,xdep0)
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
      xbev= da(13+7*nbin+4)!bottom track error velocity [cm/s]
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
         xu(i)= da(13+2*nbin+4+i)
         xv(i)= da(13+3*nbin+4+i)
         xw(i)= da(13+4*nbin+4+i)
         xev(i)=da(13+5*nbin+4+i)
         xco(i)=da(13+1*nbin+4+i)
         xei(i)=da(13+i)          !echo intensity
           if(xbd.eq.9999.0)then
            xei(i)=da(13+i)
           elseif( (xdep0+xbd).lt.(xdep0+xd(i)) )then
            xei(i)=9999.0
           endif
      enddo
C
C
      if( (abs(dmax-xdep0).LT.500.0).AND.
     &    (xdep0.LT.9999.0).AND.
     &    (xbu .LT.9999.0).AND.
     &    (xbv .LT.9999.0).AND.
     &    (xbw .LT.9999.0).AND.
     &    (xbev.LT.9999.0).AND.
     &    (abs(xpi).LT.10.0).AND.(abs(xro).LT.10.0)
     &)then
      write(20,7)da(1),time,xdep0,9.,xbu/10.,xbv/10.,xbw/10.,xbev/10.,
     &           9999.0,9999.0,xhe,xpi,xro
      write(*,7)da(1),time,xdep0,9.,xbu/10.,xbv/10.,xbw/10.,xbev/10.,
     &           9999.0,9999.0,xhe,xpi,xro
      end if
C
C
      if(time.lt.tbtm1)then
         nfile=21
      elseif(time.ge.tbtm1.and.time.lt.tbtm2)then
         nfile=22
      elseif(time.gt.tbtm2)then
         nfile=23
      endif
C
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
c      write(*,7)time,xdep0+xd(i),i+0.,xu(i),xv(i),xw(i),xev(i),
c     &              xei(i),xco(i),xhe,xpi,xro
      write(nfile,7)da(1),time,xdep0+xd(i),i+0.,xu(i)/10.,xv(i)/10.,
     &              xw(i)/10.,xev(i)/10.,xei(i),xco(i),xhe,xpi,xro
      endif
      enddo
C
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
      SUBROUTINE EI(dmax)
*REMOVE REFLECTION AT THE BOTTOM
      open(21,file=
     &'./DAT/LADCP1/001.vdw',
     &           form='formatted',status='old')
      open(22,file=
     &'./DAT/LADCP1/001.vbt',
     &           form='formatted',status='old')
      open(23,file=
     &'./DAT/LADCP1/001.vup',
     &           form='formatted',status='old')
      open(31,file=
     &'./DAT/LADCP1/001.edw',
     &           form='formatted')
      open(32,file=
     &'./DAT/LADCP1/001.ebt',
     &           form='formatted')
      open(33,file=
     &'./DAT/LADCP1/001.eup',
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

      write(*,*)'NMAX= ',n
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
      SUBROUTINE EIFILE
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   
      DIMENSION sum2(1000),sum3(1000),nko2(1000)
     &,nko3(1000),d(1000)
*Grid number
      nxmax=1000
*SET Bin number
      b0=3.0
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
     &     './DAT/LADCP1/001.edw',
     &     form='formatted',status='old')
      open(20,file=
     &     './DAT/echo.dw',
     &     form='formatted')
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
