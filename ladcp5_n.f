      program   ladcp5_n
****************************************************
* shear cal from vel profiles
* average shears with 5m-grids
*
* last revised on Oct/8/2011
****************************************************
* 1dbarごとに内挿した流速からシアを求める
* 5m grid
      implicit  none
      integer   nn
      character fname*7,UD*2
C
      UD='up'
      fname='004.vup'
C
      open(40,file=
     &     'tmp.txt',form='formatted')

      CALL RDFILE(fname,UD)

      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE(fname,UD)
      DIMENSION  u(200),v(200),z(200)
      DIMENSION  ko(7000),sum(7000),sum2(7000),g(7000)
      CHARACTER  sta*3,UD*2
      character fname*7
* ファイル名入力
      open(10,file=
     &     '../ANA/LADCP4/'//fname//'',
     &form='formatted',status='old')
      open(20,file=
     &     '../ANA/LADCP5/'//fname//'',
     &     form='formatted')
*-------------------
      do n=1,7000
         sum(n)=0.0
         sum2(n)=0.0
         ko(n)=0
      enddo
      read(10,*)xn0,z(1),u(1),v(1),xnn
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* データ読み込み開始
      do n=1,1000000
         do k=2,200
            read(10,*,end=1000)xn,z(k),u(k),v(k)
            if(xn.GT.xn0)then
               nbin=k-1
               CALL SHEAR5M(nbin,z,u,v,xn0,sum,sum2,ko,g)
               u(1)=u(nbin+1)
               v(1)=v(nbin+1)
               z(1)=z(nbin+1)
               xn0=xn
               go to 100
            end if
         end do
 100     continue
      end do
 1000 continue
      close(10)
*
      write(*,*)' LASTN= ',n,xn0
*
      do n=1,7000
         if(ko(n).ne.0)then
         write(20,'(4f10.3)')g(n),sum(n)/ko(n),sum2(n)/ko(n)
     &           ,ko(n)+0.0
         endif
      enddo
      close(20)
*
      RETURN
      END
**   *   *   *   *   *   *   *   *   *   *   *   *   *
************************************************
      SUBROUTINE SHEAR5M(nmax,z,data,data2,xens,sum,sum2,ko,g)
      DIMENSION z(200),data(200),data2(200),ko(7000),
     &     sum(7000),sum2(7000),g(7000)
     &    ,xi(200),xid1(200),xid2(200)
*GRID 5m
      do i=1,7000
      g(i)=(i-1)*5.0
      end do
      lasti=0
*
      do n=1,nmax
         do i=1,7000
         if(z(n).eq.g(i))then
            lasti=1+lasti
            xid1(lasti)=data(n)
            xid2(lasti)=data2(n)
            xi(lasti)=i
            endif
         enddo
      enddo
*
      if(lasti.ge.2)then
      do i=1,lasti-1
C         write(*,'(2f10.2)')g(xi(i)),i+0.0
         sum(xi(i))=sum(xi(i))+(xid1(i+1)-xid1(i))
         sum2(xi(i))=sum2(xi(i))+(xid2(i+1)-xid2(i))
         ko(xi(i))=ko(xi(i))+1
         write(40,'(3f10.2)')g(xi(i)),(xid1(i+1)-xid1(i)),xi(i)
      enddo
      endif
*
      RETURN
      END
*******************************************
