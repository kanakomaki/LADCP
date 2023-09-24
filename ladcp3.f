      program   ladcp3
*------------------------------------------------
* W�λĺ��ˤ�륯�饤�ƥꥪ��
* W�ĺ���¦��ʿ�Ѥ�ɸ���к�������ץ����
* Last update 2004/5/7
*20050610 BIN2 data�Ϥ���������ǽ�������ꤽ�������ƥ��饤�ƥꥪ������
*20050610��ľ���桪��
*20060207��ľ���Ϥ��� ɸ���к�4��->3��
*-------------------------------------------------
      implicit  none
      integer   nb(0:5),n,nn,k,nko,na(50),nterm,nsta
      real      a(50),dmax,xsig,xsum,
     &          xhei1,xhei2,xhei3,xhen1,xhen2,xhen3
      character sta*3,UPDW*2
*
* Ĵ�٤�ǡ����Υǥ��쥯�ȥ�����(DW,UP,BT)
      UPDW='dw'


* ¦����¬������
      nb(0)=0
      nb(1)=1

* �ҥ��ȥ����ե�����
c      open(20,file=
c     &'./DAT/LADCP3/WZAMEMO.'//UPDW//'',
c     &    form='formatted')
*
* �׻�����
      do n=1,1
*
*****1���ܤ�ɸ���к������
* �������
      nterm=1
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do
      do k=1,50
      a(k)=-49.0+2.0*(k-1.0)
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION ̾�ɤ߽Ф�+W�ĺ����ɤ߹���
*    *1���ܤ�ʿ��
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 100
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei1,xhen1,na,a)
      write(*,*)nterm,'  ',sta,nko,xsum
 100  continue
      end do
      xhei1=xsum/nko
      write(*,*)xhei1,'  = 1���ܤ�ʿ��'

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 200
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei1,xsig,nko,xhen1)
      write(*,*)nterm,'  ',sta,xhei1,xsig,xhen1
 200  continue
      end do
*    *1���ܤ�ɸ���к�
      xhen1=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen1,'  (',xsig,')  ','= 1���ܤ�ɸ���к� '
      write(20,*)n,' ¬���ֹ��1���ܡ�'
      do k=1,49
      write(20,*)a(k)+1.0,na(k)
      end do
      write(20,*)xhei1,xhen1

******2���ܤ�ɸ���к������
* �������
      nterm=2
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION ̾�ɤ߽Ф�+EV���ɤ߹���
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 300
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei1,xhen1,na,a)
      write(*,*)nterm,'  ',sta,nko,xsum
 300  continue
      end do
      xhei2=xsum/nko
      write(*,*)xhei2,' = 2���ܤ�ʿ�� '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 400
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei2,xsig,nko,xhen1)
      write(*,*)nterm,'  ',sta,xhei2,xsig,xhen1
 400  continue
      end do
*    *2���ܤ�ɸ���к�
      xhen2=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen2,xsig,' = 2���� ɸ���к�'
      write(20,*)n,'   2����'
      do k=1,49
      write(20,*)a(k)+1.0,na(k)
      end do
      write(20,*)xhei2,xhen2

*****3���ܤ�ɸ���к������
* �������
      nterm=3
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION ̾�ɤ߽Ф�+EV���ɤ߹���
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 500
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei2,xhen2,na,a)
      write(*,*)nterm,'  ',sta,xhei2,xsig,xhen2
 500  continue
      end do
      write(*,*)xsum,nko
      xhei3=xsum/nko
      write(*,*)xhei3,' = 3���ܤ�ʿ�� '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 600
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei3,xsig,nko,xhen2)
      write(*,*)nterm,'  ',sta,xhei3,xsig,xhen2
 600  continue
      end do

*    *3���ܤ�ɸ���к�
      xhen3=sqrt(xsig/(nko-1))   
      write(20,*)xhei3,xhen3
      write(*,*)xhei3,xhen3,' = 3���ܤ�ʿ�ѡ�ɸ���к�'
*
      write(*,*)'¦��   ',n,' ��λ!'
      end do

      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE READST(nn,UPDW,sta,dmax,nsta)
      CHARACTER  sta*3,UPDW*2
**  ���ơ������̾,��,����,���٤��ɤ߹���ץ����
      open(15,file='./staname',
     &   form='formatted',status='old')
*�������
      sta='/   /'
      nsta=0

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
      write(*,*)'station C',sta,
     &'    ����',xlat,'   ����',xlon
 100  continue
      close(15)
* �ǡ�����̵��¬��

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE1(nterm,UPDW,sta,
     &nko,xsum,xhei0,xhen,na,a)
      REAL*8     xt0,xt
      CHARACTER  sta*3,UPDW*2
      DIMENSION  na(50),a(50),xw(41)
**  LADCP2/UP/ DATA���ɤ߹���ץ����
**  xt0,xt�Ͼ������ʲ�5��ޤǤ���Τǡ�REAL����
**  ���󥵥�֥�ʿ�Ѥ����
      open(10,file=
     &'./DAT/LADCP2/'//sta//'.'//UPDW//'',
     &    form='formatted',status='old')
* ������� a(k):�ҥ��ȥ���� xt0:������xt�ν����
      do k=1,50
      a(k)=-49.0+2.0*(k-1.0)
      end do
**
      read(10,*)xn0,xt0,xm,xi,xm,xm,xw(1),xm,xm,xm,xm,xm,xm
      do x=1,1000000
      do j=2,40
      read(10,*,end=100)xn,xt,xm,xi,xm,xm,xw(j),xm,xm,xm,xm,xm,xm

        if(xn.NE.xn0)then
           goto 50
        else
           goto 90
        end if

*term=1�ǻ��Ѥ��륢�󥵥�֥�ʿ����xhei�����
 50   xsum0=0.0
      nko0=0
      do i=1,j-1
      if(abs(xw(i)).LT.9990.0)then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei1=xsum0/nko0
*   term=2 �ǻ��Ѥ���ʿ����xhei�����
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei2=xsum0/nko0
*   term=3 �ǻ��Ѥ���ʿ����xhei�����
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei3=xsum0/nko0

* �ĺ������
      do i=1,j-1
*   term��1 �ˤϡ����٤ƤΥǡ�������ĺ������
      if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0))then
*       * ���󥵥�֥�ʿ��xhei����λĺ�
        xsum=xsum+(xw(i)-xhei1)
        nko=nko+1
      end if

*       * �ĺ��Υҥ��ȥ����
        do k=1,49
        if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0).AND.
     &((xw(i)-xhei1).LT.a(k+1)).AND.((xw(i)-xhei1).GE.a(k))
     &)then
        na(k)=na(k)+1
        end if
        end do


*   term��2 �ˤϡ�ɸ���к���3�ܤ�����ƻĺ������
      if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
        xsum=xsum+(xw(i)-xhei2)
        nko=nko+1
      end if

*       * �ĺ��Υҥ��ȥ����
          do k=1,49
          if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0)
     &.AND.((abs(xw(i)-xhei1)-xhei0).LT.4*xhen).AND.
     &((xw(i)-xhei2).LT.a(k+1)).AND.((xw(i)-xhei2).GE.a(k))
     &)then
          na(k)=na(k)+1
          end if
          end do


*   term��3 �ˤϡ�ɸ���к���1�ܤ�����ƻĺ������
      if((nterm.EQ.3).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
        xsum=xsum+(xw(i)-xhei3)
        nko=nko+1
      end if
      end do

*   ���Υ��󥵥�֥�Τ��������
           xn0=xn
           xw(1)=xw(j)
           goto 95

 90   end do

 95   end do
 100  continue
      close(10)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE2(nterm,UPDW,sta,xhei0,xsig,nko,xhen)
      REAL*8     xt
      CHARACTER  sta*3,UPDW*2
      DIMENSION  xw(41),xt(41),xn(41),x1(41),x2(41),x3(41),x4(41),
     &x5(41),x6(41),x7(41),x8(41),x9(41),x10(41)
**  LADCP2/UP/ DATA���ɤ߹���ץ����
**  ���󥵥�֥�ʿ�Ѥ����
      open(10,file=
     &'./DAT/LADCP2/'//sta//'.'//UPDW//'',
     &    form='formatted',status='old')
      if(nterm.EQ.3)then
      open(30,file=
     &'./DAT/LADCP3/'//sta//'.'//UPDW//'',
     &    form='formatted')
      end if
* ������� ������xt�ν����
 1    format(f7.1,f12.5,2x,f7.1,2x,f4.1,2x,9(f8.2,x))
      read(10,*)xn(1),xt(1),x1(1),x2(1),x3(1),x4(1),xw(1),
     &x5(1),x6(1),x7(1),x8(1),x9(1),x10(1)

      do x=1,1000000
      do j=2,40
      read(10,*,end=100)xn(j),xt(j),x1(j),x2(j),x3(j),x4(j),
     &xw(j),x5(j),x6(j),x7(j),x8(j),x9(j),x10(j)
        if(xn(j).NE.xn(1))then
           goto 50
        else
           goto 90
        end if

*term=1�ǻ��Ѥ��륢�󥵥�֥�ʿ����xhei�����
 50   xsum0=0.0
      nko0=0
      do i=1,j-1
      if(abs(xw(i)).LT.9990.0)then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei1=xsum0/nko0



*   term=2 �ǻ��Ѥ���ʿ����xhei�����
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei2=xsum0/nko0


*   term=3 �ǻ��Ѥ���ʿ����xhei�����
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei3=xsum0/nko0

* �ĺ������
      do i=1,j-1
*   term��1 �ˤϡ����٤ƤΥǡ�������ĺ������
      if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0))then
*       * ���󥵥�֥�ʿ��xhei����λĺ�
        xsig=xsig+((xw(i)-xhei1)-xhei0)**2
        nko=nko+1
      end if



*   term��2 �ˤϡ�ɸ���к����ܤ�����ƻĺ������
      if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
        xsig=xsig+((xw(i)-xhei2)-xhei0)**2
        nko=nko+1
      end if




*   term��3 �ˤϡ�ɸ���к����ܤ�����ƻĺ������
      if((nterm.EQ.3).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
        xsig=xsig+((xw(i)-xhei3)-xhei0)**2
        nko=nko+1
        write(30,1)xn(i),xt(i),x1(i),x2(i),x3(i),x4(i),
     &xw(i),x5(i),x6(i),x7(i),x8(i),x9(i),x10(i)
      end if
      end do

*   ���Υ��󥵥�֥�Τ��������
           xn(1)=xn(j)
           xt(1)=xt(j)
           xw(1)=xw(j)
           x1(1)=x1(j)
           x2(1)=x2(j)
           x3(1)=x3(j)
           x4(1)=x4(j)
           x5(1)=x5(j)
           x6(1)=x6(j)
           x7(1)=x7(j)
           x8(1)=x8(j)
           x9(1)=x9(j)
           x10(1)=x10(j)
           goto 95

 90   end do

 95   end do
 100  continue
      close(10)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
