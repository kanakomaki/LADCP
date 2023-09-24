      program   ladcp6
** ボトムトラックから絶対流速を求めるプログラム
** 
** last update 2004/5/18
****************************************************
      implicit  none
      character sta*3,kokai*6
      integer   nn
      real      dmax
*
* 航海名 入力
      kokai='KH0301'
*計算開始
      do nn=1,100

*Station名 入力
      CALL READST(kokai,nn,sta,dmax)
CC      データがない測点
CC      if((sta.EQ.'096').OR.(sta.EQ.'098')
CC     &.OR.(sta.EQ.'006').OR.(sta.EQ.'007')
CC     &.OR.(sta.EQ.'010').OR.(sta.EQ.'009')
CC     &)then
CC       goto 1000
CC      end if


*ボトムトラック絶対流速プロファイルにするプログラム
* 向きと磁極は補正する
      CALL RDBT(kokai,sta,nn)

* ボトムトラック絶対流速を、深さごとに平均と標準偏差を出す
* 平均との差が大きすぎるものを省くプログラム
* 生のボトムトラック絶対流速の 標準偏差の3倍、2倍を除いて平均する 
      CALL RDFILE(kokai,sta,dmax)
 
1000  end do

      STOP
      END
**************************************************
      SUBROUTINE READST(kokai,nn,sta,dmax)
      REAL*8     tsa
      CHARACTER  sta*3,kokai*6
**  ステーション名,月,緯度,経度,時間差を読み込むプログラム
      open(5,file='/home/ocg/kanae/'//kokai//'/staname',
     &   form='formatted',status='old')
      sta='000'
 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2,2x,f8.5,x,f4.0)
      if(nn.EQ.1)then
      goto 50
      end if
      do n=1,nn-1
      read(5,*,end=100)
      end do
 50   read(5,1,end=100)sta,month,n1,x1,n2,x2,tsa,dmax
      xlat=n1+x1/60.0
      xlon=n2+x2/60.0
      write(*,*)'station ',sta,'日付',month,'月',
     &'    緯度',xlat,'   経度',xlon,'最大深度圧力',dmax
 100  continue
      close(5)

      if(sta.EQ.'000')then
       stop
      end if

      RETURN
      END
**************************************************
*** *******  *****  *******  *****  ***** ****  *****
      SUBROUTINE RDBT(kokai,ctds,nn)
      CHARACTER  ctds*3,kokai*6
      REAL*8     x1,x2,xt0
      DIMENSION  x1(40,12),x2(5000,40,12)
* ボトムトラックを絶対流速に直すプログラム
      open(10,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP1/'//ctds//'.BTR',
     &           form='formatted',status='old')
      open(11,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/BTR/'//ctds//'.BTR',
     &           form='formatted',status='new')
* 初期設定
      k=0
      lastk=0

      do n=1,1000000

*     *format(time,depth,xbu,xbv,xbw,xbev,xbei,xbc,xhe,xpi,xro)
      read(10,*,end=100)xt0,xd,xbu,xbv,xbw,xbev,xbei,xbc,xhe,xpi,xro

*     サブルーチン (流速データ呼び出し)
      CALL RDLADCP3(kokai,ctds,xt0,nbingo,x1,lasti)

      if(nbingo.EQ.0)then
      goto 90
      end if

      k=k+1
      do i=1,lasti
      x2(k,i,1)=xt0
      x2(k,i,2)=x1(i,2)
      x2(k,i,3)=x1(i,3) 
      x2(k,i,4)=x1(i,4)-xbu 
      x2(k,i,5)=x1(i,5)-xbv 
      x2(k,i,6)=x1(i,6)-xbw 
      x2(k,i,7)=x1(i,7)
* * 磁極の補正（xmd [radian]）をする場合  *  *  *  *
C      xxu0=x2(k,i,4)
C      xxv0=x2(k,i,5)
C      Pi=4*ATAN2(1.0,1.0)
C      xmd=(7.0)/360.0*2*PI
C      xxu1=xxu0*cos(xmd)+xxv0*sin(xmd)
C      xxv1=(-xxu0*sin(xmd))+xxv0*cos(xmd)
*  *  *  *  *  *  *  *  *  *  *  *  *

* ファイル書き込み
      write(11,'(f11.5,3x,6f7.2)')(x2(k,i,j),j=1,7)
      end do


  90  end do
 100  continue
      lastk=k
 
      close(10)
      close(11)

      RETURN
      END
*** *** ****  ** ***  *** ****  ** ***  ** *** ** **  ** ***
      SUBROUTINE RDLADCP3(kokai,ctds,xt0,nbingo,x,lasti)
      CHARACTER  ctds*3,kokai*6
      DIMENSION  x(40,12)
      REAL*8     xt0,xt1,x
* ボトムトラックに対応する相対流速を探すプログラム
* 初期設定
      nbingo=0
      lasti=0
      i=0

      open(21,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/DW/'//ctds//'',
     &           form='formatted',status='old')     
      do n=1,1000000
      read(21,*,end=100)xt1,xdep,xbin,xu,xv,xw,xev,xei,xc,xhe,xpi,xro
      if(abs(xt0-xt1).LE.0.00001)then
      i=1+i
      x(i,1)=xt1
      x(i,2)=xdep
      x(i,3)=xbin
      x(i,4)=xu
      x(i,5)=xv
      x(i,6)=xw
      x(i,7)=xev
      x(i,8)=xei
      x(i,9)=xc
      nbingo=1
      end if
      end do
 100  continue

* * データが無い測点に対しての道案内 * *
C      if(ctds.EQ.'066')then
C       goto 200
C      elseif(ctds.EQ.'012')then
C       goto 300
C      end if
* *  * *

      open(22,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/BT/'//ctds//'',
     &           form='formatted',status='old')
      do n=1,1000000
      read(22,*,end=200)xt1,xdep,xbin,xu,xv,xw,xev,xei,xc,xhe,xpi,xro
      if(abs(xt0-xt1).LE.0.00001)then
      i=1+i
C      write(*,*)xt1,xt0,i
      x(i,1)=xt1
      x(i,2)=xdep
      x(i,3)=xbin
      x(i,4)=xu
      x(i,5)=xv
      x(i,6)=xw
      x(i,7)=xev
      x(i,8)=xei
      x(i,9)=xc
      nbingo=1
      end if
      end do
 200  continue

      open(23,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/UP/'//ctds//'',
     &           form='formatted',status='old')
      do n=1,1000000
      read(23,*,end=300)xt1,xdep,xbin,xu,xv,xw,xev,xei,xc,xhe,xpi,xro
      if(abs(xt0-xt1).LE.0.00001)then
      i=1+i
      x(i,1)=xt1
      x(i,2)=xdep
      x(i,3)=xbin
      x(i,4)=xu
      x(i,5)=xv
      x(i,6)=xw
      x(i,7)=xev
      x(i,8)=xei
      x(i,9)=xc
      nbingo=1
      end if
      end do
 300  continue

      lasti=i

      close(21)
      close(22)
      close(23)

      RETURN
      END
*** *** ****  ** ***  *** ****  ** ***  ** *** ** **  ** ***
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE(kokai,sta,dmax)
* 生のボトムトラック絶対流速の 標準偏差の3倍、2倍を除いて平均する
      DIMENSION xu(200000),xv(200000),xz(200000)
      CHARACTER  sta*3,kokai*6

      open(20,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/BTR/'//sta//'.BTR',
     &form='formatted',status='old')
      n=0

      do nn=1,1000000
      read(20,*,end=100)xt,y1,xbin,x1,x2,x3,x4
      n=n+1
      xu(n)=x1
      xv(n)=x2
      xz(n)=y1
      end do
 100  continue
      close(20)
      lastn=n
      write(*,*)'lastn=',lastn

* 平均、標準偏差を求め、不良データを切る
      open(31,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/BTR/'//sta//'.HST',
     &   form='formatted',status='new')
      nbai=3
      CALL SHEAR1(xz,xu,xv,lastn,nbai)

* 再び  平均、標準偏差を求める
      nbai=2
      CALL SHEAR1(xz,xu,xv,lastn,nbai)
      close(31)
* 平均を求め、ファイルする
      open(30,file=
     &'/home/ocg/kanae/'//kokai//'/LADCP3/BTR/'//sta//'.AVR',
     &   form='formatted',status='new')
      if(lastn.EQ.0)then
      write(30,'(a8)')'NOBOTTOM'
      close(30)
      goto 999
      end if
      CALL SUMUP1(dmax,xz,xu,xv,lastn)

*
 999  continue
      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *

*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE SHEAR1(xz,xu,xv,lastn,nbai)
      DIMENSION xsumu(400),xsumv(400),nkou(400),nkov(400),
     &          d(400),xu(lastn),xv(lastn),xz(lastn)
     &         ,xsau(400),xsav(400),xavrv(400),xavru(400)
     &         ,a(50),na(400,50)
* 初期設定
      do n=1,400
      xsumu(n)=0.0
      xsumv(n)=0.0 
      nkou(n)=0
      nkov(n)=0
      xsau(n)=0.0
      xsav(n)=0.0
      xavru(n)=0.0
      xavrv(n)=0.0
      do k=1,50
      na(n,k)=0
      a(k)=-49.0+2.0*(k-1)
      end do
      end do
* グリッド設定
      do n=1,400
      d(n)=20.0*n
      end do
* 平均の計算
      write(*,*)' 平均 計算中'
      do k=1,lastn
      do n=1,400
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(xu(k).LT.9999.0))then
      xsumu(n)=xsumu(n)+xu(k)
      nkou(n)=nkou(n)+1
      end if
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(xv(k).LT.9999.0))then
      xsumv(n)=xsumv(n)+xv(k)
      nkov(n)=nkov(n)+1
      end if
*     *ヒストグラムの計算
        do j=1,49
        if((abs(xu(k)).LT.9990.0)
     &  .AND.(xu(k).LT.a(j+1)).AND.(xu(k).GE.a(j)))then
        na(n,j)=na(n,j)+1
        end if
        end do
      end do   
      end do

      do n=1,400
      if(nkou(n).gt.0)then
      xavru(n)=(xsumu(n)/nkou(n))
      write(*,'(f10.2,i10)')xavru(n),nkou(n)
      end if
      if(nkov(n).gt.0)then
      xavrv(n)=(xsumv(n)/nkov(n))
      end if
      end do

* 分散xsaの計算
      write(*,*)' 分散計算中'
      do k=1,lastn
      do n=1,400
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(nkou(n).NE.0).AND.(xu(k).LT.9999.0))then
      xsau(n)=(xu(k)-xavru(n))**2.0+xsau(n) 
      end if
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(nkov(n).NE.0).AND.(xv(k).LT.9999.0))then
      xsav(n)=(xv(k)-xavrv(n))**2.0+xsav(n)
      end if
      end do   
      end do

* 標準偏差xsaの計算
      write(*,*)' 標準偏差計算中'
      do n=1,400
       if(nkou(n).GE.2)then
       xsau(n)=sqrt(xsau(n)/(nkou(n)-1))
       else
       xsau(n)=9999.0
       end if
       if(nkov(n).GE.2)then
       xsav(n)=sqrt(xsav(n)/(nkov(n)-1))
       else
       xsav(n)=9999.0
       end if
      end do

* ヒストグラムファイル入力
      write(31,*)'1回目'
      do n=1,400
      if(xsau(n).LT.9999.0)then
      write(31,'(3f10.2)')d(n),xsau(n),xavru(n)
      write(31,'(20f6.1)')((1.0+a(j)),j=15,25)
      write(31,'(20i6)')(na(n,j),j=15,25)
      write(31,'(20f6.1)')((1.0+a(j)),j=26,36)
      write(31,'(20i6)')(na(n,j),j=26,36)
      end if
      end do
* 元データの切り落とし U
      write(*,*)'2回目 元データUの切り落とし計算中'
      CALL RMSHEAR1(xz,xu,lastn,xsau,xavru,nbai)
* 元データの切り落とし V
      write(*,*)'2回目 元データVの切り落とし計算中'
      CALL RMSHEAR1(xz,xv,lastn,xsav,xavrv,nbai)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RMSHEAR1(xz,xu,lastn,xsau,xavru,nbai)
      DIMENSION d(400),xu(lastn),xz(lastn)
     &         ,xsau(400),xavru(400)
* 元データの切り落とし 標準偏差3倍
* グリッド設定
      do n=1,400
      d(n)=20.0*n
      end do

*
      do k=1,lastn
      do n=1,400
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1)))then
       goto 50
      else 
       goto 100
      end if

 50   if(abs(xu(k)-xavru(n)).GT.nbai*xsau(n))then
       xu(k)=9999.0
      end if

 100  continue
      end do
      end do

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *

*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE SUMUP1(dmax,xz,xu,xv,lastn)
      DIMENSION d(400),xu(lastn),xv(lastn),xz(lastn)
     &         ,xsumu(400),xsumv(400),nkou(400),nkov(400)
     &         ,xavrv(400),xavru(400),v(400),u(400)
     &         ,g(500),gdatau(500),gdatav(500)
*      CHARACTER sta*3

 1    format(f6.1,2(2x,f8.2,2x,i4))
 2    format(f6.1,2x,f8.2,2x,f8.2)


* 初期設定
      do n=1,400
      xsumu(n)=0.0
      xsumv(n)=0.0 
      nkou(n)=0
      nkov(n)=0
      xavru(n)=0.0
      xavrv(n)=0.0
      u(n)=0.0
      v(n)=0.0
      end do
* グリッド設定
      do n=1,400
      d(n)=20.0*n
      end do

* 平均の計算
      write(*,*)'平均 計算中'
      do k=1,lastn
      do n=1,400
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(xu(k).LT.9999.0))then
      xsumu(n)=xsumu(n)+xu(k)
      nkou(n)=nkou(n)+1
      end if
      if((xz(k).GT.d(n)).AND.(xz(k).LE.d(n+1))
     &.AND.(xv(k).LT.9999.0))then
      xsumv(n)=xsumv(n)+xv(k)
      nkov(n)=nkov(n)+1
      end if
      end do
 100  continue
      end do

      do n=1,400
      if(nkou(n).gt.0)then
      xavru(n)=(xsumu(n)/nkou(n))
      else
      xavru(n)=9999.0
      end if
      if(nkov(n).gt.0)then
      xavrv(n)=(xsumv(n)/nkov(n))
      else
      xavrv(n)=9999.0
      end if
      end do

* 流速書出し     
      do n=1,400
      if((xavru(n).LT.9999.0).AND.(xavrv(n).LT.9999.0))then 
      write(30,1)0.5*(d(n)+d(n+1)),xavru(n),nkou(n),xavrv(n),nkov(n)
      end if
      end do


      close(30)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
**************************************************
      SUBROUTINE NAISO(nmax,z,data,g,gdata,igmax)
      DIMENSION z(nmax),data(nmax),g(500),gdata(500)
** グリッドをつくり、それに合わせてまばらなデータを
** 内挿&外挿して補間するプログラム
      gkan=20.0
      write(*,*)gkan,'m   グリッド間隔'
      write(*,*)z(1),data(1),'    始点'
      write(*,*)z(nmax),'     終点'

      do i=1,500
      g(i)=gkan*(i-1)+20.0
      if(z(nmax).GT.g(i))then
         igmax=i+1
      end if
      end do
      write(*,*)g(igmax),'グリッド終点'

      do i=1,igmax
      do n=1,nmax-1
      if(g(i).LT.z(1))then
      gdata(i)=data(1)-
     &((data(2)-data(1))/(z(2)-z(1)))*(z(1)-g(i))
      end if

      if(g(i).GE.z(nmax))then
      gdata(i)=data(nmax)+
     &((data(nmax)-data(nmax-1))/(z(nmax+1)-z(nmax)))*(g(i)-z(nmax))
      end if

      if((g(i).LT.z(n+1)).AND.(g(i).GE.z(n)))then
      gdata(i)=data(n)+
     &((data(n+1)-data(n))/(z(n+1)-z(n)))*(g(i)-z(n))
      end if
      end do
      end do

c      do i=1,igmax
c      write(*,*)g(i),gdata(i)
c      end do

      RETURN
      END
*******************************************
