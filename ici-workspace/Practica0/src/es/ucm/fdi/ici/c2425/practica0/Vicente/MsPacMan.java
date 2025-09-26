package es.ucm.fdi.ici.c2425.practica0.Vicente;

import java.awt.*;
import java.util.*;

import pacman.controllers.PacmanController;
import pacman.game.Constants;
import pacman.game.Constants.GHOST;
import pacman.game.Constants.MOVE;
import pacman.game.Game;
import pacman.game.GameView;

public class MsPacMan extends PacmanController{

    @Override
    public MOVE getMove(Game game, long timeDue)
    {
        float limit = 100;
        GHOST nearestGhost = getNearestGhost(limit, game);

        if (nearestGhost == null)
        {
            int nearestPill = getNearestPill(game);

            GameView.addPoints(
                    game,
                    Color.yellow,
                    game.getShortestPath(
                            nearestPill,
                            game.getPacmanCurrentNodeIndex()));

            return game.getApproximateNextMoveTowardsTarget(
                        game.getPacmanCurrentNodeIndex(),
                        nearestPill,
                        game.getPacmanLastMoveMade(),
                        Constants.DM.PATH);
        }

        if(Boolean.TRUE.equals(game.isGhostEdible(nearestGhost)))
        {
            GameView.addPoints(
                    game,
                    Color.green,
                    game.getShortestPath(
                            game.getGhostCurrentNodeIndex(nearestGhost),
                            game.getPacmanCurrentNodeIndex()));

            return game.getApproximateNextMoveTowardsTarget(
                        game.getPacmanCurrentNodeIndex(),
                        game.getGhostCurrentNodeIndex(nearestGhost),
                        game.getPacmanLastMoveMade(),
                        Constants.DM.PATH);
        }
        else
        {
            GameView.addPoints(
                    game,
                    Color.red,
                    game.getShortestPath(
                            game.getGhostCurrentNodeIndex(nearestGhost),
                            game.getPacmanCurrentNodeIndex()));

            return game.getApproximateNextMoveAwayFromTarget(
                    game.getPacmanCurrentNodeIndex(),
                    game.getGhostCurrentNodeIndex(nearestGhost),
                    game.getPacmanLastMoveMade(),
                    Constants.DM.PATH);
        }
    }

    public String getName() {
    	return "MsPacMan";
    }

    private GHOST getNearestGhost(float limit, Game game)
    {
        GHOST nearestGhost = null;

        for(GHOST ghost : GHOST.values())
        {
            if(game.getGhostLairTime(ghost) <= 0)
            {
                int ghostNode = game.getGhostCurrentNodeIndex(ghost);
                int pacmanNode = game.getPacmanCurrentNodeIndex();
                float dis = game.getShortestPathDistance(ghostNode, pacmanNode);

                if (dis < limit) {
                    limit = dis;
                    nearestGhost = ghost;
                }
            }
        }

        return nearestGhost;
    }

    private int getNearestPill(Game game)
    {
        int pacman = game.getPacmanCurrentNodeIndex();

        Queue<Integer> queue = new LinkedList<>();
        boolean[] visited = new boolean[game.getNumberOfNodes()];

        queue.add(pacman);
        visited[pacman] = true;

        while (!queue.isEmpty())
        {
            int current = queue.poll();

            int pillIndex = game.getPillIndex(current);
            int powerPillIndex =  game.getPowerPillIndex(current);
            if((pillIndex != -1 && Boolean.TRUE.equals(game.isPillStillAvailable(pillIndex))) ||
                    (powerPillIndex != -1 && Boolean.TRUE.equals(game.isPowerPillStillAvailable(powerPillIndex))))
                return current;

            int[] neighbours = game.getNeighbouringNodes(current);
            for (int neighbour : neighbours)
            {
                if (!visited[neighbour])
                {
                    visited[neighbour] = true;
                    queue.add(neighbour);
                }
            }
        }

        /* Creo que este return de "no hay pill" no tiene uso real porque asumo que el juego cambia
           de nivel antes de que getMove intente moverse sin pills en el tablero
        */
        return -2;
    }
}
