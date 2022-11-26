program exerA
implicit none
real(8) :: t, dt, v_0, v !declarando variáveis do problema
integer :: i, n !parâmetros para contagem dos loops
real(8), parameter :: P = 400d0, m = 80d0 !parâmetros constantes do problema

print*, 'Insira aqui o tempo total T, o intervalo delta t e a velocidade inicial v0:'
read(*, *) t, dt, v_0

n = int(t / dt) !número total de iterações
v = v_0 !velocidade inicial setada para início do loop

open(1, file = 'velA_out.dat', status = 'replace')

write(1, *) '       0                  ', v_0

do i = 1, n
    v = v + (P/(m * v)) * dt !calculando a velocidade após dt
    write(1,*) (i * dt), v !escrevendo cada linha da tabela
end do

close(1)

end program exerA