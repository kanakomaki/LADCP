      program   ladcpvel_for_fitting
* 船底ADCPにFITTINGさせる用の流速プロファイルを求める
* 
* last 2006/04/17
      implicit  none
      integer   nn,lastn
      real      u(0:7000),v(0:7000),z(0:7000)
      character sta*3,UD*2
C
      UD='dw'
c      UD='up'
C
      do nn=1,1
* STATION
      CALL READST(nn,sta)
* 
      CALL RDFILE(sta,UD,z,u,v,lastn)
      open(20,file='./DAT/RELATIVEVEL/'//sta//'.'//UD//''
     &,form='formatted')
      CALL MK_FILE(z,u,v,lastn)
      end do
C
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
 50   read(15,*,end=100)sta,month,n1,x1,n2,x2,dmax
      xlat=n1+x1/60.0
      xlon=n2+x2/60.0
      write(*,*)'station C',sta
 100  continue
      close(15)
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE(sta,UD,z,u,v,lastn)
      DIMENSION  u(0:7000),v(0:7000),z(0:7000)
      CHARACTER sta*3,UD*2
* ファイル名入力
      open(10,file=
     &     './DAT/LADCP4/'//sta//'.s5m'//UD//'',
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
            write(*,*)z(n),zs
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
