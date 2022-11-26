program exerC2
implicit none
real(8) :: t, dt, w_0, w, theta_0, theta, l, m, E !declarando variáveis do problema
integer :: i, n !parâmetros para contagem dos loops
real(8), parameter :: g = 9.81d0, pi = 4.0d0 * atan(1.0d0) !parâmetros constantes do problema

print*, 'Insira aqui o ângulo inicial theta_0:'
read(*, *) theta_0
print*, 'Insira aqui o comprimento da haste l:'
read(*, *) l
print*, 'Insira aqui a massa m:'
read(*, *) m
print*, 'Insira aqui o intervalo delta t entre cada iteração:'
read(*, *) dt
print*, 'Insira aqui o tempo total T:'
read(*, *) t

theta_0 = theta_0 * (pi / 180.0d0) !conversão de graus para radioanos

n = int(t / dt) !número total de iterações

w_0 = 0.0d0 !velocidade angular inicial igual a zero

!calculando a energia total inicial (cinética + potencial gravitaciol)
E = (m * (w_0**2) * (l ** 2)) / 2.0d0 + m * g * l * (1 - cos(theta_0))

open(1, file = 'exerC2_out.dat', status = 'replace') !criando tabela de theta em função de t
OPEN(2, file = 'energiaC2_out.dat', status = 'replace') !criando tabela de energia em função de t para o gráfico

write(1, *) '      0                  ', theta_0

write(2,*) '      0                  ', E

do i = 1, n

    w = w_0 - (g / l) * theta_0 * dt !calculando a velocidade angular após dt

    theta = theta_0 + w * dt !!calculando o ângulo de oscilação após dt pelo método de Euler-Cromer

    w_0 = w !atualizando w_i para o próximo loop

    if (theta > pi) then !mantendo o ângulo theta entre -pi e +pi

        theta = theta - 2 * pi

    else if (theta < -  pi) then

        theta = theta + 2 * pi

    end if

    theta_0 = theta !atualizando theta_i para o próximo loop
    
    !calculando a energia total após dt
    E = (m * (w**2) * (l ** 2)) / 2.0d0 + m * g * l * (1 - cos(theta))

    write(1,*) (i * dt), theta !escrevendo as linhas da tabela de theta
    write(2,*) (i * dt), E !escrevendo as linhas da tabela de energia
end do

close(1)
close(2)

end program exerC2