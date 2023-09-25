      program   ladcp3_n
****************************************************
* cut data of large w vel (> 3 sigma in each profile)
*
* last revised on Oct/8/2011
****************************************************
      implicit  none
      integer k,imax,nmax,l
      integer na(60)
      real    D(8,32768),
     &        a(60),uave,usig,WENS(32768)
      character fname*7
      common /vel/ D(8,32768),imax
      common /histo/a(3,60),na(3,60)
      common /wens/ WENS(32768)


      fname='004.vup'

* SET
      do l=1,3
      do k=1,60
      a(l,k)  = 5.0 * (k-1)
      na(l,k)= 0
      end do
      end do
* DAT
      CALL READRAW(fname)
* Error Velocity -term1
      CALL AVERAGE_W_ENS
      CALL HISTOGRAM(1)
      CALL AVERAGE(uave)
      CALL SIGMA(uave,usig)
      CALL REMOVE(uave,usig,3)
* Error Velocity -term2
      CALL AVERAGE_W_ENS
      CALL HISTOGRAM(2)
      CALL AVERAGE(uave)
      CALL SIGMA(uave,usig)
      CALL REMOVE(uave,usig,3)
      CALL WRITE(fname)
*
      END
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE AVERAGE_W_ENS
      common /vel/ D(8,32768),imax
      common /wens/ WENS(32768)
C         
      do i=1,100
      if(D(6,i).ne.9999.)then
         sumw= D(6,i)
         ko  = 1
         istart =i+1
         k1 = i
         goto 10
      endif
      enddo
C
 10   continue
      do I=Istart,imax
         if(D(6,i).eq.9999.)then
            goto 100
         endif
         if(D(1,i).eq.D(1,i-1))then
            sumw =sumw + D(6,i)
            ko   = ko  + 1
         else
            do k=k1,i-1
            wens(k)=sumw/ko
            enddo
c            write(*,*)wens(k1),k1,i-1
            sumw = D(6,i)
            ko=1
            k1=i
         endif
 100  continue
      end do
C
      write(*,*)'average_w_ens completed'
      write(*,*)
C
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *

* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE READRAW(fname)
      common /vel/ D(8,32768),imax
      character fname*7
c
      OPEN(5,file=
     &'../ANA/LADCP2/'//fname//'',form='formatted',status='old')
c
      imax = 0
      do I=1,50000
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
      SUBROUTINE HISTOGRAM(l)
      real    da,a
      common /vel/ D(8,32768),imax
      common /histo/a(3,60),na(3,60)
      common /wens/ WENS(32768)
      do k=1,60
         na(l,k)=0
      enddo
      da = a(l,2)-a(l,1)
      do i=1,imax

         phi = (D(6,i)-wens(i))

         do k=1,60
            if(phi.ge.(a(l,k)-da).and.phi.lt.(a(l,k)+da))then
               na(l,k) =na(l,k) +1
            endif
         enddo
      enddo
c
      write(*,*)'historgram completed',l
      write(*,*)
c
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE AVERAGE(uave)
      common /vel/ D(8,32768),imax
      common /wens/ WENS(32768)
      sumu=0.0
      nmax=0
      do I=1,imax
         if(D(6,i).ne.9999.)then
c            write(*,*)wens(i),D(6,i)
         sumu = sumu + abs(D(6,i)-wens(i))
         nmax = nmax + 1
         endif
      end do
C
      uave = sumu / nmax
      write(*,*)'average completed'
      write(*,*) uave, nmax
      write(*,*)
C
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *

* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE SIGMA(uave,usig)
      common /vel/ D(8,32768),imax
      common /wens/ WENS(32768)
      usig=0.0
      nmax =0
      do I=1,imax
         if(D(6,i).ne.9999.)then
            usig=(abs(D(6,i)-wens(i))-uave)**2 + usig
            nmax = nmax +1
         endif
      end do
      usig=sqrt(usig/(nmax-1))
      write(*,*)'sigma, completed'
      write(*,*)uave,usig
      write(*,*)
C
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *

* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE REMOVE(uave,usig,nbai)
      common /vel/ D(8,32768),imax
      common /wens/ WENS(32768)
      nko =0
      do I=1,imax
         if(D(6,i).ne.9999.and.
     &  abs(abs(D(6,i)-wens(i))-uave).gt.(nbai*usig))then
            nko =nko+1
            nmax =nmax -1
            do k=1,8
               D(k,i) = 9999.
            enddo
         endif
      end do
C
      write(*,*)'data remove completed'
      write(*,*) nko
      write(*,*)
C
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *

* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE WRITE(fname)
      character fname*7
      common /vel/ D(8,32768),imax
 7    format(f6.0,2x,f7.1,2x,f3.0,2x,5(f8.2,x))
      nmax =0
c
      OPEN(10,file=
     &'../ANA/LADCP3/'//fname//'',form='formatted')
c
      do I=1,imax
      if(D(7,i).ne.9999.)then
      write(10,7) (D(j,i),j=1,8)
      nmax =nmax+1
      endif
      end do
 100  continue
      close(10)
      write(*,*)'write completed'
      write(*,*)'data number ',nmax
      write(*,*)'file name = ',fname
      write(*,*)
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
