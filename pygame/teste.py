import pygame
from rich import inspect


pygame.init()
tela = pygame.display.set_mode((1000, 700))
pygame.display.set_caption('Teste Mudança de Título')
rodando = True
while rodando:
    for evento in pygame.event.get():
        pygame.draw.circle(tela,  (0, 0, 255), center=(200, 200), radius=100)
        if evento.type == pygame.QUIT:
            rodando = False
    pygame.display.update()
pygame.quit()
