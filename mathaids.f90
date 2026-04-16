module mathaids
        implicit none

        abstract interface
                double complex function compNDimFunc(val)
                        double complex, dimension(:), intent(in) :: val
                end function
        end interface

        abstract interface
                double precision function reNDimFunc(val)
                        double precision, dimension(:), intent(in) :: val
                end function
        end interface
        
        integer, parameter :: dp = kind(1d0)
        double complex :: j = (0d0, 1d0)
        double complex :: e = cmplx(exp(1d0), 0d0, kind=dp)

        contains

                function eye(n) result(arr) ! identity matrix generator
                        integer, intent(in) :: n
                        double complex :: arr(n,n)
                        integer :: i
                        arr = 0d0
                        do i = 1, n
                                arr(i, i) = (1d0, 0d0)
                        end do
                end function eye

                subroutine linspace(x0, xN, nPoints, arr) ! linspace
                        double precision, intent(in) :: x0, xN
                        integer, intent(in) :: nPoints
                        double precision, intent(out) :: arr(nPoints)
                        integer :: i
                        arr = 0d0
                        do i = 1, nPoints
                                arr(i) = x0 + (i - 1)*(xN - x0) / (nPoints - 1)
                        end do
                end subroutine linspace

                function cross_product(x, y) result(z)
                        double precision, dimension(3), intent(in) :: x, y
                        double precision, dimension(3) :: z
                        z(1) = x(2)*y(3) - x(3)*y(2)
                        z(2) = x(3)*y(1) - x(1)*y(3)
                        z(3) = x(1)*y(2) * x(2)*y(1)
                end function cross_product

                subroutine solveGeneralSystem(A, b, x, n, info) ! solves A*x = b square systems, no requirements on matrix structure
                        integer, intent(in) :: n
                        double complex, dimension(n, n), intent(in) :: A
                        double complex, dimension(n), intent(in) :: b
                        integer, intent(out) :: info
                        double complex, dimension(n), intent(out) :: x
                        double complex, dimension(n, n) :: A1
                        double complex, dimension(n) :: xOut
                        integer, dimension(n) :: piv
                        A1 = A
                        xOut = b
                        call ZGESV(n, 1, A1, n, piv, xOut, n, info)
                        if (info /= 0) then
                                print *, info, ", ZGESV fail"
                                return
                        end if
                        x = xOut
                end subroutine solveGeneralSystem

                subroutine solveSPD(A, b, x, n, info) ! solves SPD system A*x = b
                        integer, intent(in) :: n
                        double complex, dimension(n, n), intent(in) :: A
                        double complex, dimension(n), intent(in) :: b
                        double complex, dimension(n), intent(out) :: x
                        integer, intent(out) :: info
                        double complex, dimension(n, n) :: A1
                        double complex, dimension(n) :: xOut
                        A1 = A
                        xOut = b
                        call ZPOSV('U', n, 1, A1, n, xOut, n, info)
                        if (info > 0) then
                                print *, "not SPD"
                        else if (info < 0) then
                                print *, "illegal value"
                        end if
                        x = xOut
                end subroutine solveSPD

                subroutine solveGeneralEig(A, B, eigVals, eigVecs, n) ! solves (A - lambda*B)*v = 0, eigenvectors are eigVecs(:, i)
                        integer, intent(in) :: n
                        double precision, dimension(n, n), intent(in) :: A, B
                        double complex, dimension(n, n), intent(out) :: eigVecs
                        double complex, dimension(n), intent(out) :: eigVals
                        double precision, dimension(n, n) :: A1, B1, vecs
                        double precision, dimension(n) :: eigValRe, eigValIm, eigValD
                        integer :: workVal, info, i, j
                        double precision, allocatable :: workVec(:)
                        double precision :: workQ(1)
                        A1 = A
                        B1 = B
                        call DGGEV('N', 'V', n, A1, n, B1, n, eigValRe, eigValIm, eigValD, (1,1), 1, vecs, n, workQ, -1, info)
                        workVal = int(workQ(1))
                        allocate(workVec(workVal))
                        call DGGEV('N', 'V', n, A1, n, B1, n, eigValRe, eigValIm, eigValD, (1,1), 1, vecs, n, workVec, workVal, info)
                        if (info /= 0) print *, "DGGEV fail"
                        do i = 1, n
                                if (abs(eigValD(i)) > n*epsilon(1d0)) then
                                        eigVals(i) = cmplx(eigValRe(i) / eigValD(i), eigValIm(i) / eigValD(i), kind=dp)
                                else
                                        eigVals(i) = 1010101
                                end if
                        end do
                        j = 1
                        do while (j <= n)
                                if (abs(eigValIm(j)) <= 0d0 + epsilon(1d0)) then
                                        eigVecs(:,j) = cmplx(vecs(:,j), 0d0, kind=dp)
                                        j = j + 1 
                                else
                                        eigVecs(:,j) = cmplx(vecs(:,j), vecs(:,j + 1), kind=dp)
                                        eigVecs(:,j + 1) = cmplx(vecs(:,j), -vecs(:,j + 1), kind=dp)
                                        j = j + 2
                                end if
                        end do
                        deallocate(workVec)
                end subroutine solveGeneralEig

                subroutine findGrad(f, x, dx, n, h) ! finds gradient at vector x, dimension n, step size h
                        procedure(reNDimFunc) :: f
                        integer, intent(in) :: n
                        double precision, intent(in) :: h
                        double precision, dimension(n), intent(in) :: x
                        double precision, dimension(n), intent(out) :: dx
                        double precision, dimension(n) :: e
                        integer :: i

                        do i = 1, n
                                e = 0d0
                                e(i) = 1d0
                                dx(i) = (f(x + h*e) - f(x)) / h
                        end do
                end subroutine findGrad

                subroutine findGrad2(f, x, dx, n) ! complex differencing, more accurate
                        procedure(compNDimFunc) :: f
                        integer, intent(in) :: n
                        double complex, dimension(n), intent(in) :: x
                        double complex, dimension(n), intent(out) :: dx
                        double complex, dimension(n) :: e
                        double complex :: h = (1d-100, 0)
                        integer :: i

                        do i = 1, n
                                e = (0d0, 0d0)
                                e(i) = j
                                dx(i) = AIMAG(f(x + h*e) / h)
                        end do
                end subroutine findGrad2

                subroutine optimize(f, x, n, tol, stepLim) ! bfgs optimizer, initial guess x, step size limit
                        procedure(compNDimFunc) :: f
                        double complex, dimension(n), intent(inout) :: x
                        integer, intent(in) :: n
                        double complex, intent(in) :: tol, stepLim
                        double complex, dimension(n) :: dx, y, s, p, gradCheck
                        double complex :: a, c1, c2
                        double complex, dimension(n, n) :: B
                        logical :: armijo, curvature, converged
                        integer :: counter, info
                        B = eye(n)
                        info = 0
                        c1 = (1d-4, 0d0)
                        c2 = (0.9d0, 0d0)
                        counter = 0
                        converged = .false.
                        do while (converged .eqv. .false.)
100                             counter = counter + 1
                                call findGrad2(f, x, dx, n)
                                call solveSPD(B, -dx, p, n, info)
                                if (norm2(real(p)) > real(stepLim)) then
                                        p = p * stepLim/(norm2(real(p)))
                                end if
                                a = (1d0, 0d0)
                                armijo = .false.
                                curvature = .false.
                                do while ((armijo .eqv. .false.) .and. (curvature .eqv. .false.))
                                        if (real(a) < 1d-3) then
                                                B = eye(n)
                                                GO TO 100
                                        end if
                                        call findGrad2(f, x + a*p, gradCheck, n)
                                        if (real(f(x + a*p)) <= real(f(x) + c1*a*dot_product(dx, p))) then 
                                                armijo = .true.
                                        end if
                                        if (real(dot_product(gradCheck, p)) >= real(c2*dot_product(dx, p))) then
                                                curvature = .true.
                                        end if
                                        if ((armijo .eqv. .false.) .or. (curvature .eqv. .false.)) then
                                                a = 0.9*a
                                        end if
                                end do
                                s = a*p
                                x = x + s
                                gradCheck = dx
                                call findGrad2(f, x, dx, n)
                                y = dx - gradCheck
                                if (info == 0) then
                                        B = B + (matmul(reshape(y, [n, 1]), reshape(y, [1, n])) / dot_product(y, s)) - &
                                        (matmul(matmul(B, reshape(s, [n, 1])), &
                                        matmul(reshape(s, [1, n]), transpose(B))) / dot_product(s, matmul(B, s)))
                                else if (info > 0) then
                                        B = eye(n)
                                else
                                        print *, "ZPOSV fail"
                                        return
                                end if
                                if (norm2(real(dx)) <= real(tol)) then
                                        converged = .true.
                                end if
                       end do
                end subroutine          

                function inv(A, n) result (Ainv) ! returns square matrix inverse
                        integer, intent(in) :: n
                        double complex, dimension(n, n), intent(in) :: A
                        double complex, dimension(n, n) :: Ainv, M
                        double complex, allocatable :: workVec(:)
                        double complex :: workQ(1)
                        integer :: info, workVal
                        integer, dimension(n, n) :: piv

                        M = A
                        call ZGETRF(n, n, M, n, piv, info)
                        if (info /= 0) then
                                print *, "ZGETRF failed"
                        end if
                        call ZGETRI(n, M, n, piv, workQ, -1, info)
                        workVal = int(workQ(1))
                        allocate(workVec(workVal))
                        call ZGETRI(n, M, n, piv, workVec, workVal, info)
                        if (info /= 0) then
                                print *, "ZGETRI failed"
                        else
                                Ainv = M
                        end if
                        deallocate(workVec)
                end function inv

end module mathaids
