package es.ucm.fdi.ici.c2526.practica1.grupoB;

import pacman.game.Game;

public class Utils {

    /**
     * Busca la primera interseccion desde el nodo dado
     *
     * @param node Nodo desde el que busca
     * @param parent Padre del nodo para evitar buscar por donde viene el pacman

     * @return El id del nodo de la interseccion
     */
    static public int firstJunctionFrom(int node, int parent, Game game)
    {
        int current = node;
        int prev = parent;
        int steps = 0;
        int maxSteps = game.getNumberOfNodes();

        while (steps < maxSteps)
        {
            if (game.isJunction(current)) return current;

            int[] neighbours = game.getNeighbouringNodes(current);
            int next = -1;
            for (int n : neighbours)
            {
                if (n != prev) { next = n; break; }
            }
            prev = current;
            current = next;

            steps++;
        }
        return -2;
    }

}
