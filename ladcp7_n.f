      program   ladcp7_n
****************************************************
* cal relative vel profile from shears with 5m-grids
*
* last revised on Oct/8/2011
****************************************************
      implicit  none
      integer   nn,lastn
      real      u(0:7000),v(0:7000),z(0:7000)
      character fname*7
C
      fname='004.vup'
C
* 
      CALL RDFILE(fname,z,u,v,lastn)
      open(20,file='../ANA/LADCP7/'//fname//''
     &,form='formatted')
      CALL MK_FILE(z,u,v,lastn)
*
      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE(fname,z,u,v,lastn)
      DIMENSION  u(0:7000),v(0:7000),z(0:7000)
      CHARACTER fname*7

      open(10,file='../ANA/LADCP5/'//fname//'',
     &   form='formatted',status='old')
*
      n=0
      do i=1,10000
         read(10,*,end=100)zs,us,vs,xko
         if((zbtm-zs).lt.5.0)then
            n=n+1
            z(n)=zs+5.0
            u(n)=u(n-1)+us
            v(n)=v(n-1)+vs
            write(*,*)z(n),zs,u(n),v(n)
        endif
      end do
 100  continue
      lastn=n
*
      z(0)=z(1)-5.0
      u(0)=0.0
      v(0)=0.0
*
      write(*,*)'ZBTM= ',zbtm,sta,'SHEAR MAX=',zs
      close(10)
      RETURN
      END
**   *   *   *   *   *   *   *   *   *   *   *   *   *

*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE MK_FILE(z,u,v,lastn)
      DIMENSION  u(0:lastn),v(0:lastn),z(0:lastn)
C
      do n=0,lastn
         write(20,'(3f10.3)')z(n),u(n),v(n)
      enddo
      close(20)
C
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
