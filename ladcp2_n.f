      program   ladcp2_n
****************************************************
* cut data of large error vel (> 3 sigma)
*
* last revised on Oct/8/2011
****************************************************
      implicit  none
      integer k,imax,nmax,l
      integer na(60)
      character fname*7
      real    D(8,32768),
     &        a(60),uave,usig
      common /vel/ D(8,32768),imax,nmax
      common /histo/a(3,60),na(3,60)
* SET
      fname='001.vup'

      do l=1,3
      do k=1,60
      a(l,k)  = 5.0 * (k-1)
      na(l,k)= 0
      end do
      end do
* DAT
      CALL READRAW(fname)
* Error Velocity -term1
      CALL HISTOGRAM(1)
      CALL AVERAGE(uave)
      CALL SIGMA(uave,usig)
      CALL REMOVE(uave,usig,3)
* Error Velocity -term2
      CALL AVERAGE(uave)
      CALL SIGMA(uave,usig)
      CALL REMOVE(uave,usig,3)
* Error Velocity -term3
      CALL AVERAGE(uave)
      CALL SIGMA(uave,usig)
      CALL WRITE(fname)
*
      stop
      END
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE READRAW(fname)
      common /vel/ D(8,32768),imax,nmax
      common /histo/a(3,60),na(3,60)
      character fname*7
c
      OPEN(5,file=
     &'../ANA/LADCP1/'//fname//'',form='formatted',status='old')
c
      imax = 0
      do I=1,5000000
      read(5,*,end=100) D(1,i),x,(D(j,i),j=2,8)
      write(*,*)D(1,i)
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
      common /vel/ D(8,32768),imax,nmax
      common /histo/a(3,60),na(3,60)
      do k=1,60
         na(l,k)=0
      enddo
      da = a(l,2)-a(l,1)
      do i=1,imax
         phi = D(7,i)
         do k=1,60
            if(phi.ge.(a(l,k)-da).and.phi.lt.(a(l,k)+da))then
               na(l,k) =na(l,k) +1
            endif
         enddo
      enddo
c
      write(*,*)'historgram completed'
      write(*,*)
c
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * 
      SUBROUTINE AVERAGE(uave)
      common /vel/ D(8,32768),imax,nmax
      sumu=0.0
      nmax=0
      do I=1,imax
         if(D(7,i).ne.9999.)then
         sumu = sumu + D(7,i)
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
      common /vel/ D(8,32768),imax,nmax
      usig=0.0
      do I=1,imax
         if(D(7,i).ne.9999.)then
            usig=(D(7,i)-uave)*(D(7,i)-uave) + usig
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
      common /vel/ D(8,32768),imax,nmax
      nko =0
      do I=1,imax
         if(D(7,i).ne.9999.and.abs(D(7,i)-uave).gt.(nbai*usig))then
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
      common /vel/ D(8,32768),imax,nmax
 7    format(f6.0,2x,f7.1,2x,f3.0,2x,5(f8.2,x))

c
      OPEN(10,file=
     &'../ANA/LADCP2/'//fname//'',form='formatted')
c
      do I=1,imax
      if(D(7,i).ne.9999.)then
      write(10,7) (D(j,i),j=1,8)
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
