      program   ladcpnaiso
* 1dbarごとに流速を内挿する
* 磁極の補正（md度）
*      xmd=0.0 xmd=10.0(c17-c53)
* last 2006/10/09
      implicit  none
      integer   nn
      character sta*3,UD*2
C
      UD='dw'
C
      do nn=1,1
* STATION INFORMATION
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
c 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2)
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
      DIMENSION  da(13),u(41),v(41),z(41)
      REAL*8 da
      CHARACTER sta*3,UD*2
* ファイル名入力
      open(10,file=
     &     './DAT/LADCP3/'//sta//'.'//UD//'',
     &form='formatted',status='old')
      open(20,file=
     &     './DAT/LADCP4/'//sta//'.1m'//UD//''
     &,form='formatted')

* データ 1行目
      read(10,*)(da(j),j=1,13)

*     磁極の補正（md度）
      xmd=0.0
      Pi=4*ATAN2(1.0,1.0)
      u(1)=da(6)*sin(xmd/180.0*pi)+da(5)*cos(xmd/180.0*pi)
      v(1)=da(6)*cos(xmd/180.0*pi)+(-da(5)*sin(xmd/180.0*pi))
      z(1)=da(3)
      xn0=da(1)

* データ読み込み開始
      do n=1,20000
         do k=2,40
            read(10,*,end=1000)(da(j),j=1,13)
            
*           磁極の補正（md度）
            u(k)=da(6)*sin(xmd/180.0*pi)+da(5)*cos(xmd/180.0*pi)
            v(k)=da(6)*cos(xmd/180.0*pi)+(-da(5)*sin(xmd/180.0*pi))
            z(k)=da(3)
            if(da(1).GT.xn0)then
               nbin=k-1
               CALL NAISO(nbin,z,u,v,xn0)
               u(1)=u(nbin+1)
               v(1)=v(nbin+1)
               z(1)=z(nbin+1)
               xn0=da(1)
               go to 100
            end if
         end do
 100     continue
      end do
 1000 continue
      close(10)
*
      write(*,*)'C',sta,' LASTN＝',n
*
      RETURN
      END
**   *   *   *   *   *   *   *   *   *   *   *   *   *
**************************************************
      SUBROUTINE NAISO(nmax,z,data,data2,xens)
      DIMENSION z(41),data(41),data2(41),g(500),
     &     gdata(500),gdata2(500)
** 1mグリッドに合わせてまばらなデータを
** 内挿するプログラム
      if(nmax.eq.1)then
         goto 1000
      endif
      z01=z(1)
      z02=z(nmax)
      CALL MAKEINT(z01,z02,z1,z2)
*
      gkan=1.0
c      write(*,*)gkan,'  グリッド間隔'
c      write(*,*)z1,data(1),' 始点'
c      write(*,*)z2,data(nmax),' 終点'
      do i=1,500
         g(i)=gkan*(i-1)+z1
         if(z2.EQ.g(i))then
            igmax=i
         end if
      end do
c      write(*,*)g(igmax),'  グリッド終点'
*naiso start
      do i=1,igmax
      do n=1,nmax-1
      if(g(i).LT.z(n+1).AND.g(i).GE.z(n))then
      gdata(i)=data(n)+
     &((data(n+1)-data(n))/(z(n+1)-z(n)))*(g(i)-z(n))

      gdata2(i)=data2(n)+
     &((data2(n+1)-data2(n))/(z(n+1)-z(n)))*(g(i)-z(n))
C      write(*,*)i,g(i),z(n),z(n+1),igmax
      end if
      end do
      end do
C
      do i=1,igmax
         write(20,'(4f10.2,i5)')xens,g(i),gdata(i),gdata2(i),nmax
C         write(*,'(4f10.2,i5)')xens,g(i),gdata(i),gdata2(i),nmax
      enddo
C      stop
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
