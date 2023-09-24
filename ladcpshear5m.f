      program   ladcpshear5m
* 1dbarごとに内挿した流速からシアを求める
* 5m grid
* last 2006/02/03
      implicit  none
      integer   nn
      character sta*3,UD*2
C
      UD='dw'
C
      open(40,file=
     &     'tmp.txt',form='formatted')

      do nn=1,1
* STATION
      CALL READST(nn,sta)
      CALL RDFILE(sta,UD)
      end do

      STOP
      END
**************************************************
      SUBROUTINE READST(nn,sta)
      CHARACTER  sta*3
**  ステーション名,月,緯度,経度を読み込むプログラム
      open(15,file='./staname',
     &   form='formatted',status='old')
      sta='/   /'
 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2)
      if(nn.EQ.1)then
      goto 50
      end if
      do n=1,nn-1
      read(15,*,end=100)
      end do
 50   read(15,*,end=100)sta,month,n1,x1,n2,x2
      xlat=n1+x1/60.0
      xlon=n2+x2/60.0
      write(*,*)'station C',sta
 100  continue
      close(15)
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE(sta,UD)
      DIMENSION  u(200),v(200),z(200)
      DIMENSION  ko(7000),sum(7000),sum2(7000),g(7000)
      CHARACTER sta*3,UD*2
* ファイル名入力
      open(10,file=
     &     './DAT/LADCP4/'//sta//'.1m'//UD//'',
     &form='formatted',status='old')
      open(20,file=
     &     './DAT/LADCP4/'//sta//'.s5m'//UD//'',
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
            read(10,*,end=1000)xn,z(k),u(k),v(k),xnn
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
      write(*,*)'C',sta,' LASTN＝',n,xn0
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
