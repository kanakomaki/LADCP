      program   ladcp6_n
****************************************************
* cal absolute vel from btm track vel data
* average absolute bottom vel with 5m-grids
* mag correlation (md [deg])
*
* last revised on Oct/8/2011
****************************************************
      implicit  none
      integer   imax
      real      U(32768),V(32768),W(32768),Z(32768),G(20)
      character fname1*10,fname2*7
      common /BT/ U(32768),V(32768),W(32768),Z(32768),imax
      common /G/ G(20)
* SET
      fname1='004.vbt_dw'
      fname1='004.vbt_up'

      fname2='004.btr'

      open(30,file=
     &'../ANA/LADCP6/'//fname1//'',form='formatted')

      CALL RD_DATA(fname1,fname2)
      CALl MAG_HOSEI(7.0)

      CALL MAKEGRID
      CALL AVERAGE

 
      STOP
      END
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE AVERAGE
      common /BT/ U(32768),V(32768),W(32768),Z(32768),imax
      common /G/ G(20)
      do k=1,20

      sumu=0.0
      sumv=0.0
      sumw=0.0
      nmax=0

      do I=1,imax
         if(U(i).ne.9999..and.(abs(Z(i)-g(k)).le.5. ))then
         sumu = sumu + U(i)
         sumv = sumv + V(i)
         sumw = sumw + W(i)
         nmax = nmax + 1
         endif
      end do
C
      uave = sumu / nmax
      vave = sumv / nmax
      wave = sumw / nmax
      if(nmax.gt.1)then
      write(*,*)'average completed'
      write(*,*) g(k), uave, nmax
      write(*,*) g(k), vave, nmax
      write(*,*) g(k), wave, nmax
      write(*,*)
      write(30,'(5f10.2)')g(k),uave,vave,wave,nmax+0.0
      endif
C
      enddo
C
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE MAKEGRID
      common /BT/ U(32768),V(32768),W(32768),Z(32768),imax
      common /G/ G(20)
      do i=1,imax
         sumz = sumz + z(i)
      enddo
      avrz = sumz / imax
      g0 = int(avrz/5.) * 5.0
      do k=1,20
         g(k)=g0 + (5.0*(k-10))
      enddo
      write(*,*)
      write(*,*)'g0=', g0, imax
      write(*,*)
      RETURN
      END
*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE RD_DATA(fname1,fname2)
      CHARACTER  fname1*10,fname2*7
      common /BT/ U(32768),V(32768),W(32768),Z(32768),imax

      open(10,file=
     &'../ANA/LADCP1/'//fname1//'',
     &           form='formatted',status='old')
      imax = 0
      do i=1,1000000
        read(10,*,end=100)xen0,x,xdep,x,xu,xv,xw,xev
        
        do k=1,1000000
           open(11,file=
     &          '../ANA/LADCP1/'//fname2//'',
     &          form='formatted',status='old')
           read(11,*,end=90)xen,x,x,x,xbu,xbv,xbw,xbev
           if(xen0.eq.xen.and.abs(xbev).lt.20.)then
              goto 90
           else
              xbu =999.
              xbv =999.
              xbw =999.
           endif
        enddo
 90     continue
        close(11)
        if(abs(xbu).lt.999.and.abs(xev).lt.20.)then
           imax = imax +1
           u(imax) = xu + xbu
           v(imax) = xv + xbv
           w(imax) = xw + xbw
           z(imax) = xdep
        endif
      enddo
 100  continue
      write(*,*)
      write(*,*)'imax=', imax
      write(*,*)
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MAG_HOSEI(xmd)
      common /BT/ U(32768),V(32768),W(32768),Z(32768),imax
      
      Pi=4*ATAN2(1.0,1.0)
      do i=1,imax
      if(U(i).ne.9999.)then
      u1=U(i)*sin(xmd/180.0*pi)+  V(i)*cos(xmd/180.0*pi)
      v1=U(i)*cos(xmd/180.0*pi)+(-V(i)*sin(xmd/180.0*pi))
      U(i)=u1
      V(i)=v1
      endif
      enddo
      write(*,*)'magnetic hosei completed'
      write(*,*)
       RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
