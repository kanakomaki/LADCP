      program   ladcp8_n
****************************************************
* cal absolute vel profile from relative profile & reference vel (b-track)
* use least squares method (or single adjustment)
*
* last revised on Oct/8/2011
****************************************************
      implicit  none
      integer nmax
      character fname*7,fname2*10
      real    bz(100),bu(100),bv(100)
      common /BTM/ bz(100),bu(100),bv(100),nmax

      fname ='004.vup'
      fname2='004.vbt_up'

      CALL RDBTM_PROFILE(fname2)
      CALL RD(fname,fname2)

      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *

*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  
      SUBROUTINE RD(fname,fname2)
*--------------------------------------------------------------------
*THIS PROGRAM IS TO
*1. INTEGRATE THE AVERAGED SHEARS AND MAKE THE RELATIVE VELOCITY PROFILE 
*2. FIT THE PROFILE WITH THE BOTTOM-REFERENCED VELOCITY
*3. FIT THE PROFILE WITH THE VADCP VELOCITY
*--------------------------------------------------------------------
      DIMENSION  z0(1000),u0(1000),v0(1000)
      DIMENSION  xd(0:8000),xhei(0:1000),xsum(0:1000)
      DIMENSION  xadu(1100),xadv(1100),xadd(1100)
      DIMENSION  z(8000),u(8000),v(8000)
      CHARACTER  fname*7,fname2*10

*READ A FILE OF THE LADCP AVERAGED vel
      open(10,file='../ANA/LADCP7/'//fname//'',
     &form='formatted',status='old')
      open(30,file='../ANA/LADCP8/'//fname//'',
     &form='formatted')
*---------------------------------------------------
      do n=1,800
      read(10,*,end=100)z0(n),u0(n),v0(n)
      write(*,*)z0(n),u0(n),v0(n)
      end do
 100  continue
      lastn=n-1
      close(10)

*INTERPOLATE THE RELATIVE VELOCITY PROFILE EVERY 1 DBAR
c      write(*,*)'ENTER THE BOTTOM DEPTH [DBAR]'
c      read(*,*)xbtm
      xbtm = z0(lastn)
      CALL NAISO(xbtm,lastn,z0,u0,z,u,igmax)
      CALL NAISO(xbtm,lastn,z0,v0,z,v,igmax)
      do n=1,5
         write(*,*)z(n),u(n),v(n)
      enddo

C
C Least square fitting with bottom track vel
C
      CALL FITTING(z,u,v,igmax, xsau,xsav)
      do n=1,igmax
        u(n)=u(n)+xsau
        v(n)=v(n)+xsav
        write(*,'(3f10.2,2f10.3)')z(n),u(n),v(n),xsau,xsav
        write(30,'(5f10.2)')z(n),u(n),v(n),xsau,xsav
      end do




*READ THE AVERAGED BOTTOM-REFERENCED VELOCITY
c      CALL RDBTM1(fname2,xbu,xbv,xbz)
c*FIT THE RELATIVE PROFILE TO THE BOTTOM-REFERENCED VELOCITY
c      nbtrack=9999
c      do n=1,igmax
c         if(abs(z(n)-xbz).LT.1.0)then
c            nbtrack=n
c            ubtrack=u(n)
c            vbtrack=v(n)
c            write(*,*)ubtrack,nbtrack,xbz,abs(z(n)-xbz),xbu,xbv
c         end if
c      end do
c      if(nbtrack.EQ.9999)then
c         write(*,*)'Stop at Line 249'
c         stop
c      endif
c      do n=1,igmax
c         u(n)=u(n)-ubtrack+xbu
c         v(n)=v(n)-vbtrack+xbv
c         write(30,'(3f10.2)')z(n),u(n),v(n)
c         write(*,'(3f10.2)')z(n),u(n),v(n)
c      end do

      RETURN
      END
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  
      SUBROUTINE RDBTM_PROFILE(fname2)
      character fname2*10
      dimension bz(100),bu(100),bv(100)
      common /BTM/ bz(100),bu(100),bv(100),nmax
* READ THE BOTTOM-REFERENCED VELOCTY PROFILE
      open(20,file='../ANA/LADCP6/'//fname2//'',
     &form='formatted',status='old')
      do n=1,50
      read(20,*,end=10)bz(n),bu(n),bv(n)
      end do
 10   continue
      close(20)
      nmax=n-1
*
      write(*,*)
      write(*,*)'bottom velocity'
      write(*,*)xbz,xbu,xbv,xnum0
      write(*,*)
      RETURN
      END
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  
      SUBROUTINE RDBTM1(fname2,xbu,xbv,xbz)
      character fname2*10

* READ THE BOTTOM-REFERENCED VELOCTY PROFILE 
      open(20,file='../ANA/LADCP6/'//fname2//'',
     &form='formatted',status='old')
      do n=1,50
      read(20,*,end=10)xbz,xbu,xbv,xnum0
      end do
 10   continue
      close(20)
*
      write(*,*)
      write(*,*)'bottom velocity'
      write(*,*)xbz,xbu,xbv,xnum0
      write(*,*)
      RETURN
      END
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  


**************************************************
      SUBROUTINE NAISO(xbtm,lastn,z,data,g,gdata,igmax)
      DIMENSION z(lastn),data(lastn),g(8000),gdata(8000)
*SETTEI (igmin= grid no siten!)
      gkan=1.0
      write(*,*)'-------------------------'
      write(*,*)gkan,'g kankaku [dbar]'
      write(*,*)z(1),data(1),'FIRST depth & data'
      write(*,*)z(lastn),'      LAST depth'
      do i=1,8000
      if(abs(i-z(1)).lt.1.0)then
         igmin=i
      end if
      end do
         igmin=igmin-20.0
      write(*,*)'G(1)= ',igmin
*make grid
      do i=1,8000
      g(i)=igmin+gkan*(i-1)
      if(abs(g(i)-xbtm).lt.1.0)then
         igmax=i
      end if
      end do
      write(*,*)g(igmax),'G(IGMAX)',xbtm
      write(*,*)'-------------------------'

*extend data
      do i=1,igmax
      do n=1,lastn-1
      if(g(i).LT.z(1))then
      gdata(i)=data(1)-
     &((data(2)-data(1))/(z(2)-z(1)))*(z(1)-g(i))
      end if

      if(g(i).GE.z(lastn))then
      gdata(i)=data(lastn)+
     &(data(lastn)-data(lastn-1))/(z(lastn)-z(lastn-1))
     &*(g(i)-z(lastn))
      end if

      if((g(i).LT.z(n+1)).AND.(g(i).GE.z(n)))then
      gdata(i)=data(n)+
     &((data(n+1)-data(n))/(z(n+1)-z(n)))*(g(i)-z(n))
      end if
      end do
      end do

      RETURN
      END
*******************************************


*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE FITTING(z,u,v,igmax,xsau,xsav)
* 最小二乗法でVADCPにFITTINGし、平行移動値を読み取る
      dimension u(igmax),v(igmax),z(igmax)
      common /BTM/ bz(100),bu(100),bv(100),nmax
*
      write(*,*)'LEAST SQUARE MEAN FITTING '
      sumu=0.0
      sumv=0.0
      sumlu=0.0
      sumlv=0.0
      nko=0
*READing reference VEL (b-track) & LADCP VEL
      do n=1,nmax
         u0=bu(n)
         v0=bv(n)
         z0=bz(n)
               do i=1,igmax
               if(abs(z0-z(i)).lt.1.0)then
               goto 50
               endif
               enddo
               goto 100
 50            continue

      write(*,*)'FITTING Depth= ',z(i),n,iv
      sumlu=u(i)+sumlu
      sumlv=v(i)+sumlv
      sumu=u0+sumu
      sumv=v0+sumv
      nko=nko+1

 100  continue
      enddo
 200  continue
*
      xsau=(sumu-sumlu)/nko
      xsav=(sumv-sumlv)/nko
      write(*,*)'xsau=',xsau
      write(*,*)'xsav=',xsav
      write(*,*)'FITTING DATA NUM= ',nko
      write(*,*)'------------------------'
*
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
