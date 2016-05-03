from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import pygame
from pygame.locals import *
import math

class GLWin:
    def __init__(self):
        pygame.init()
        self.scr = pygame.display.set_mode((640, 480), OPENGL|DOUBLEBUF)
        self.start()
        self.resize()
        self.l_drag = None
        self.r_drag = None
        self.rot = (0., 0.)
        self.tran = (0., 0., 3.)

    def start(self):
        glClearColor(0., 0., 0., 0.)
        glShadeModel(GL_FLAT)

    def resize(self):
        glViewport(0, 0, 640, 480)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity();
        gluPerspective(45.6, 4./3., .1, 100.)
        glMatrixMode(GL_MODELVIEW)

    def test(self):
        glClear(GL_COLOR_BUFFER_BIT)
        glColor3f(1., 1., 1.)
        glLoadIdentity()

        gluLookAt(0., 0., 0., 0., 0., 1., 0., 1., 0.)
        glTranslatef(self.tran[0], self.tran[1], self.tran[2])
        glRotatef(-self.rot[1], 1., 0., 0.)
        glRotatef(self.rot[0], 0., 1., 0.)

        #glutWireCube(1.)

        glColor3f(1., 0., 0.)
        glBegin(GL_LINES)
        glVertex3f(0., 0., 0.)
        glVertex3f(1., 0., 0.)
        glEnd()
        glColor3f(0., 1., 0.)
        glBegin(GL_LINES)
        glVertex3f(0., 0., 0.)
        glVertex3f(0., 1., 0.)
        glEnd()
        glColor3f(0., 0., 1.)
        glBegin(GL_LINES)
        glVertex3f(0., 0., 0.)
        glVertex3f(0., 0., 1.)
        glEnd()
        glFlush()

    def step(self):
        ret = []
        event = pygame.event.get()
        for e in event:
            if e.type == pygame.MOUSEBUTTONDOWN:
                if e.button == 1:
                    if self.l_drag == None:
                        self.l_drag = e.pos
                if e.button == 3:
                    if self.r_drag == None:
                        self.r_drag = e.pos
            elif e.type == pygame.MOUSEMOTION:
                if e.buttons[0] == 1:
                    if self.l_drag != None:
                        dx = e.pos[0]-self.l_drag[0]
                        dy = e.pos[1]-self.l_drag[1]
                        self.l_drag = e.pos
                        self.rot = (self.rot[0]+dx, self.rot[1]+dy)
                if e.buttons[2] == 1:
                    if self.r_drag != None:
                        x, y, z = self.tran
                        dx = e.pos[0]-self.r_drag[0]
                        dy = e.pos[1]-self.r_drag[1]
                        self.r_drag = e.pos
                        self.tran = (x, y+dy*.1, z+dx*.1)

            elif e.type == pygame.MOUSEBUTTONUP:
                if e.button == 1:
                    self.l_drag = None
                if e.button == 3:
                    self.r_drag = None
            elif e.type == pygame.KEYDOWN:
                ret.append(e.key)
        self.test()
        return ret

    def update(self):
        pygame.display.flip()

if __name__ == '__main__':
    w = GLWin()
    while True:
        w.step()
        w.update()
