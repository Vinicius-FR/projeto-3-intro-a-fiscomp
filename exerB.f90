program exerB
implicit none
real(8) :: t, dt, v_0, v, A, d, t_terminal, v_m !declarando variáveis do problema
integer :: i, n !parâmetros para contagem dos loops
real(8), parameter :: P = 400d0, m = 80d0, rho = 1.2d0, C = (1.0d0 / 2.0d0) !parâmetros constantes do problema

print*, 'Insira aqui o tempo total T, o intervalo delta t, a velocidade inicial v0 e a área A:'
read(*, *) t, dt, v_0, A

n = int(t / dt) !número total de iterações
v = v_0 !valores iniciais setados para início do loop
d = 0.0d0
t_terminal = 0.0d0

open(1, file = 'velB_out.dat', status = 'replace')

write(1, *) '       0                  ', v_0

do i = 1, n

    d = d + v * dt !calculando a distância percorrida por aproximação 

    if ((v == v + (P / (m * v)) * dt - (C * rho * A * v **2) * dt / m) .and. (t_terminal == 0)) then
        t_terminal = (i-1) * dt !verificando o momento em que a velocidade para de variar (velocidade terminal)
    end if

    v = v + (P / (m * v)) * dt - (C * rho * A * v **2) * dt / m !calculando a velocidade após dt

    write(1,*) (i * dt), v !escrevendo cada linha da tabela
end do

close(1)

v_m = (v_0 + v) / 2 !velocidade média

print*, 'Ao analisar a expressão para v(t) e os gráficos para diferentes valores de área A, nota-se que ', &
        'quanto menor a área de contato com a resistência do ar, menor será o retardo sofrido por essa força e ', &
        'maior será a velocidade terminal, ou seja, ao se curvar, o ciclista diminui sua área de contato, sofrendo ', &
        'uma menor resistência e atingindo uma maior velocidade. Um outro jeito mitigar o efeito dissipativo do ar ', &
        'é se aproveitar do "vácuo" criado por um grupo (pelotão) de ciclistas, fazendo com que os ciclistas do meio ', &
        'sofram um menor arrasto do ar, que é desviado pelos corredores mais à frente, reduzindo o desgaste físico. ', &
        'Um método semelhante, é ficar atrás de um outro corredor ao invés de ultrapassá-lo logo, pois, ao se aproveitar ', &
        'de um menor contato com o arrasto, atingirá uma maior velocidade, como visto nos gráficos, e terá que gastar ', &
        'menos energia para superar a resistência do ar, poupando fôlego e ficando menos cansado que o corredor da,', &
        'frente, o que torna a ultrapassagem mais fácil posteriormente e garante uma vantagem a longo prazo na corrida. ', &
        'No geral, todas essas táticas têm o objetivo de diminuir o desgaste físico e garantir uma maior durabilidade e ', &
        'velocidade durante a corrida.'
print*, 'O espaço total percorrido pelo ciclista é:', d
print*, 'A velocidade final do ciclista é:', v
print*, 'O ciclista atinge sua velocidade terminal em t =', t_terminal
print*, 'A velocidade média do ciclista é:', v_m

!função exata v(t) sem resistência do ar v(t) = (0.01 + 10 * t) ** 0.5

end program exerB