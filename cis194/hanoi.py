def hanoi(n , origem, destino, aux, i = 0):
    if n == 1:
        print ('ponta\t\t' + origem + '->' + destino)
        return

    hanoi(n-1, origem, aux, destino, i)
    print ('meio\t\t' + origem + '->' + destino)
    hanoi(n-1, aux, destino, origem, i)

hanoi(15, 'a', 'b', 'c')
