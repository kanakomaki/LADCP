      program   ladcp4_n
****************************************************
* interpolating each vel profile to 1m intervals
* mag correlation (md [deg])
*
* last revised on Oct/8/2011
****************************************************
* 1mごとに流速を内挿
* 磁極の補正(md度)
      implicit  none
      integer   imax
      real  D(8,32768)
      common /vel/ D(8,32768),imax
      character fname*7
      fname='004.vup'

      OPEN(5,file=
     &'../ANA/LADCP3/'//fname//'',form='formatted',status='old')
      OPEN(10,file=
     &'../ANA/LADCP4/'//fname//'',form='formatted')

      CALL READRAW
      CALl MAG_HOSEI(7.0)

      CALL STEP1

c      CALL WRITE

      STOP
      END
* * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE READRAW
      common /vel/ D(8,32768),imax
c
c
      imax = 0
      do I=1,5000000
         read(5,*,end=100) (D(j,i),j=1,8)
      end do
 100  continue
      imax =i-1
      close(5)
      write(*,*)'read completed'
      write(*,*)'data number ',imax
      write(*,*)'file name = ',fname
      write(*,*)
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *

* * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MAG_HOSEI(xmd)
      common /vel/ D(8,32768),imax
      
      Pi=4*ATAN2(1.0,1.0)
      do i=1,imax
      if(D(4,i).ne.9999.)then
      u1=D(4,i)*sin(xmd/180.0*pi)+D(5,i)*cos(xmd/180.0*pi)
      v1=D(4,i)*cos(xmd/180.0*pi)+(-D(5,i)*sin(xmd/180.0*pi))
      D(4,i)=u1
      D(5,i)=v1
      endif
      enddo
      write(*,*)'magnetic hosei completed'
      write(*,*)
       RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *



* * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE STEP1
      DIMENSION  u(100),v(100),z(100)
     &          , ug(500),vg(500),zg(500)
      common /vel/ D(8,32768),imax
      common /vel2/ D2(5,327680),imax2
      m=0
C
      u(1)=D(4,1)
      v(1)=D(5,1)
      z(1)=D(2,1)
      nbin=1
C
      do I=2,imax
         if(D(1,i).eq.D(1,i-1))then
            nbin=nbin+1
         elseif(D(1,i).gt.D(1,i-1).and.nbin.ge.2)then
            j=0
            do k=i-nbin,i-1
            j=j+1
            u(j)=D(4,k)
            v(j)=D(5,k)
            z(j)=D(2,k)
            ens = D(1,k)
            enddo
C
            CALL NAISO(ens,nbin,z,u,v,zg,ug,vg,kgmax)
C
c            do kg=1,kgmax
c               m=m+1
c               D2(1,m)=D(1,i-1)
c               D2(2,m)=zg(kg)
c               D2(3,m)=ug(kg)
c               D2(4,m)=vg(kg)
c               write(*,*)m,D2(1,m),D(2,m)
c            enddo


            nbin=1
         endif
 100  continue
      end do
C
      imax2 =m
C
      write(*,*)'step1 completed'
      write(*,*)
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *




* * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE NAISO(ens,nmax,z,dat1,dat2,g,gdata,gdata2,igmax)
      DIMENSION  z(*),dat1(*),dat2(*),
     &           g(500),gdata(500),gdata2(500)
      if(nmax.le.1)then
         goto 1000
      endif

      z01=z(1)
      z02=z(nmax)
c      write(*,*)z(1),z(nmax)
      CALL MAKEINT(z01,z02,z1,z2)
*
      gkan=1.
c      write(*,*)gkan,'        grid interval '
c      write(*,*)z1,dat1(1),'       data start'
c      write(*,*)z2,dat1(nmax),'    data   end'
      do i=1,500
         g(i)=gkan*(i-1)+z1
         if(z2.EQ.g(i))then
            igmax=i
         end if
      end do
c      write(*,*)g(1),g(igmax),'   grid '
      do i=1,igmax
      do n=1,nmax-1
      if(g(i).LT.z(n+1).AND.g(i).GE.z(n))then
      gdata(i)=dat1(n)+
     &         ((dat1(n+1)-dat1(n))/(z(n+1)-z(n)))*(g(i)-z(n))
      gdata2(i)=dat2(n)+
     &         ((dat2(n+1)-dat2(n))/(z(n+1)-z(n)))*(g(i)-z(n))
c     write(*,*)i,g(i),z(n),z(n+1),igmax
      end if
      end do
      end do
C
      do i=1,igmax
         write(10,'(4f10.2)')ens,g(i),gdata(i),gdata2(i)
      end do
 1000 RETURN
      END
*******************************************



**************************************************
      SUBROUTINE MAKEINT(z01,z02,z1,z2)
      DIMENSION  z(7000)
      do n=1,7000-1
         z(n)=n+0.0
      enddo
      do n=1,7000-1
         if(z01.ge.z(n).and.z01.lt.z(n+1))then
            z1=z(n+1)
            goto 100
         endif
      enddo
 100  continue
      do n=1,7000-1
         if(z02.ge.z(n).and.z02.lt.z(n+1))then
            z2=z(n)
            goto 200
         endif
      enddo
 200  continue
      RETURN
      END
**************************************************

* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE WRITE
      character fname*7
      common /vel2/ D2(5,327680),imax2
      fname='001.vdw'
 7    format(f6.0,2x,4(f8.2,x))

c
      OPEN(10,file=
     &'../ANA/LADCP4/'//fname//'',form='formatted')
c
      do I=1,imax2
         if(D2(3,i).ne.9999.)then
            write(10,7) (D2(j,i),j=1,4)
         endif
      end do
 100  continue
      close(10)
      write(*,*)'write completed'
      write(*,*)'data number ',imax2
      write(*,*)'file name = ',fname
      write(*,*)
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
