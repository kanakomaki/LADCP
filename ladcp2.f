      program   ladcp2
      implicit  none
      integer   nb(0:5),n,nn,k,nko,na(50),nterm,nsta
      real      a(50),dmax,xhei,xsig,xhen,xsum
      character sta*3,UPDW*2

*SET CAST (dw or up or bt)
*DW ~ down cast data
*UP ~ up cast data
*BT ~ near bottom data 
*
      UPDW='dw' !SET de or up or bt
*
*
      nb(0)=0
      nb(1)=1

*If you need histgram file
c      open(20,file=
c     &'./LADCP2/EVMEMO.'//UPDW//'',
c     &    form='formatted')

*
      do n=1,1

* 
* Setting
      nterm=1
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do
      do k=1,50
      a(k)=-98.0+4.0*(k-1.0)
      end do
**STATION 名読み出し+EVの読み込み
      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 100
      end if

      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xsig,xhen,na,a)
      write(*,*)nterm,'  ',sta,nko,xsum
 100  continue
      end do
      xhei=xsum/nko
      write(*,*)xhei,'  = 1回目の平均'

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 200
      end if
      CALL RDFILE2(nterm,UPDW,sta,xsig,xhei,xhen)
      write(*,*)nterm,'  ',sta,xhei,xsig,xhen
 200  continue
      end do

*    *1回目の標準偏差
      xhen=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen,'  (',xsig,')  ','= 1回目の標準偏差 '
      write(20,*)n,' 測線番号（1回目）'
      do k=1,49
      write(20,*)a(k)+2.0,na(k)
      end do
      write(20,*)xhei,xhen


******2回目の標準偏差を求める
* 初期設定
      nterm=2
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION 名読み出し+EVの読み込み
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 300
      end if

      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xsig,xhen,na,a)
      write(*,*)nterm,'TERM',sta,nko,xsum
 300  continue
      end do
      xhei=xsum/nko
      write(*,*)xhei,' = 2回目の平均 '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 400
      end if
      CALL RDFILE2(nterm,UPDW,sta,xsig,xhei,xhen)
      write(*,*)nterm,'TERM',sta,xhei,xsig,xhen
 400  continue
      end do
*    *2回目の標準偏差
      xhen=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen,xsig,' = 2回目 標準偏差'
      write(20,*)n,'   2回目'
      do k=1,49
      write(20,*)a(k)+2.0,na(k)
      end do
      write(20,*)xhei,xhen


*****3回目の標準偏差を求める
* 初期設定
      nterm=3
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION 名読み出し+EVの読み込み
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 500
      end if

      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xsig,xhen,na,a)
      write(*,*)nterm,'  ',sta,xhei,xsig,xhen
 500  continue
      end do
      write(*,*)xsum,nko
      xhei=xsum/nko
      write(*,*)xhei,' = 3回目の平均 '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 600
      end if
      CALL RDFILE2(nterm,UPDW,sta,xsig,xhei,xhen)
      write(*,*)nterm,'  ',sta,xhei,xsig,xhen
 600  continue
      end do

*    *3回目の標準偏差
      xhen=sqrt(xsig/(nko-1))   
      write(*,*)xhei,xhen,' = 3回目の平均、標準偏差'
*
      write(*,*)'側線   ',n,' 終了!'
      end do

      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE READST(nn,UPDW,sta,dmax,nsta)
      CHARACTER  sta*3,UPDW*2
**
      open(15,file='./staname',
     &   form='formatted',status='old')
* 初期設定
      sta='/   /'
      nsta=0
C 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2,2x,f8.5)
        if(nn.EQ.1)then
         goto 50
        end if
      do n=1,nn-1
      read(15,*,end=100)
      end do
 50   read(15,*,end=100)sta,month,n1,x1,n2,x2,tsa,dmax
      xlat=n1+x1/60.0
      xlon=n2+x2/60.0
      write(*,*)'station',sta,'month',month,
     &'    lat',xlat,'   lon',xlon,'max depth',dmax
 100  continue
      close(15)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE1(nterm,UD,sta,nko,xsum,xsig,xhen,na,a)
      CHARACTER  sta*3,UD*2
      DIMENSION  na(50),a(50)
**  EIU DATAを読み込むプログラム
**  平均を求める
      open(10,file=
     &'./DAT/LADCP1/'//sta//'.e'//UD//'',
     &    form='formatted',status='old')
* 初期設定
      do k=1,50
      a(k)=-98.0+4.0*(k-1.0)
      end do

      do x=1,1000000
        read(10,*,end=100)xm,xm,xm,xm,xm,xm,xm,xev,xm,xm,xm,xm,xm

*   term＝1 には、すべてのデータで求める
      if((nterm.EQ.1).AND.(abs(xev).LT.9990.0))then
        xsum=xsum+xev
        nko=nko+1
      end if
        do k=1,49
        if((nterm.EQ.1).AND.(abs(xev).LT.9990.0)
     &.AND.(xev.LT.a(k+1)).AND.(xev.GE.a(k)) )then
        na(k)=na(k)+1
        end if
        end do
*   term＝2 には、標準偏差の3倍を除く
      if((nterm.EQ.2).AND.(abs(xev).LT.9990.0).AND.
     &(abs(xev-xhei).LT.3*xhen))then
        xsum=xsum+xev
        nko=nko+1
      end if
      do k=1,49
      if((nterm.EQ.2).AND.(abs(xev).LT.9990.0)
     &.AND.(abs(xev-xhei).LT.3*xhen)
     &.AND.(xev.LT.a(k+1)).AND.(xev.GE.a(k)))then
       na(k)=na(k)+1
      end if
      end do
*   term＝3 には、標準偏差の1倍を除く
      if((nterm.EQ.3).AND.(abs(xev).LT.9990.0).AND.
     &(abs(xev-xhei).LT.1*xhen))then
        xsum=xsum+xev
        nko=nko+1
      end if

      end do
 100  continue
      close(10)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE2(nterm,UD,sta,xsig,xhei,xhen)
      CHARACTER  sta*3,UD*2
      REAL*8     x1
**
      open(10,file=
     &'./DAT/LADCP1/'//sta//'.e'//UD//'',
     &    form='formatted',status='old')
      if(nterm.EQ.3)then
      open(30,file=
     &'./DAT/LADCP2/'//sta//'.'//UD//'',
     &    form='formatted')
      end if
C
 1    format(f7.1,f12.5,2x,f7.1,2x,f4.1,2x,9(f8.2,x))
C
      do x=1,1000000
      read(10,*,end=100)xn,x1,x2,x3,x4,x5,x6,xev,x7,
     &x8,x9,x10,x11

*   term＝1 には、すべてのデータで求める
      if((nterm.EQ.1).AND.(abs(xev).LT.9990.0))then
      xsig=(xev-xhei)*(xev-xhei)+xsig
      goto 90
      else
      end if
*   term＝2 には、標準偏差の3倍を除く
      if((nterm.EQ.2).AND.(abs(xev).LT.9990.0).AND.
     &(abs(xev-xhei).LT.3*xhen))then
      xsig=(xev-xhei)*(xev-xhei)+xsig
      goto 90
      else
      end if
*   term＝3 には、標準偏差の1倍を除いて、ファイルに記録する
      if((nterm.EQ.3).AND.(abs(xev).LT.9990.0).AND.
     &(abs(xev-xhei).LT.1*xhen))then

      xsig=(xev-xhei)*(xev-xhei)+xsig
      write(30,1)xn,x1,x2,x3,x4,x5,x6,xev,x7,
     &x8,x9,x10,x11

      goto 90
      else
      end if

  90  end do
 100  continue

      close(10)

      if(nterm.EQ.3)then
      close(30)
      end if


      RETURN
      END
******************************************
