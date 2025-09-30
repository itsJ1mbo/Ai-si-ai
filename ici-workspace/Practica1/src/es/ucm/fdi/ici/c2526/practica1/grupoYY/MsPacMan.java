package es.ucm.fdi.ici.c2526.practica1.grupoYY;

import pacman.controllers.PacmanController;
import pacman.game.Constants;
import pacman.game.Constants.MOVE;
import pacman.game.Game;
import pacman.game.GameView;

import java.awt.*;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

public class MsPacMan extends PacmanController{

    @Override
    public MOVE getMove(Game game, long timeDue)
    {
        int pacman = game.getPacmanCurrentNodeIndex();
        boolean[] visited = new boolean[game.getNumberOfNodes()];

        if(game.isJunction(pacman))
        {
            Constants.MOVE[] m = game.getPossibleMoves(pacman, game.getPacmanLastMoveMade());

            float max = Float.MAX_VALUE;
            int c = 0;

            int ghost = getNearestGhost(max, game);

            if (ghost == -2)
            {
                int nearestPill = getNearestPill(game);

                return game.getApproximateNextMoveTowardsTarget(
                        game.getPacmanCurrentNodeIndex(),
                        nearestPill,
                        game.getPacmanLastMoveMade(),
                        Constants.DM.PATH);
            }

            for(int i = 0; i < m.length; i++)
            {
                int next = nextJunction(game, visited);

                float gp = game.getShortestPathDistance(ghost, pacman);
                float gc = game.getShortestPathDistance(ghost, next);
                float d = game.getShortestPathDistance(pacman, next);

                float h = (d / gc) * gp;
                if(h < max)
                {
                    max = h;
                    c = next;
                }
            }

            GameView.addPoints(
                    game,
                    Color.yellow,
                    game.getShortestPath(
                            c,
                            game.getPacmanCurrentNodeIndex()));

            return game.getApproximateNextMoveTowardsTarget(
                    game.getPacmanCurrentNodeIndex(),
                    c,
                    game.getPacmanLastMoveMade(),
                    Constants.DM.PATH);

        }

        return MOVE.NEUTRAL;
    }
    
    public String getName() {
    	return "MsPacManNeutral";
    }

    private int nextJunction(Game game, boolean[] visited)
    {
        int pacman = game.getPacmanCurrentNodeIndex();

        Stack<int[]> stack = new Stack<>();

        stack.push(new int[]{pacman, -1});
        visited[pacman] = true;

        while (!stack.isEmpty())
        {
            int[] currentPair = stack.pop();
            int current = currentPair[0];
            int parent = currentPair[1];

            if(game.isJunction(current)) return current;

            int[] neighbours = game.getNeighbouringNodes(current);
            for (int neighbour : neighbours)
            {
                if (!visited[neighbour])
                {
                    if (neighbour != parent) {
                        visited[neighbour] = true;
                        stack.push(new int[]{neighbour, current});
                    }
                }
            }
        }

        return -2;
    }

    private int getNearestGhost(float limit, Game game)
    {
        int nearestGhost = -2;

        for(Constants.GHOST ghost : Constants.GHOST.values())
        {
            if(game.getGhostLairTime(ghost) <= 0)
            {
                int ghostNode = game.getGhostCurrentNodeIndex(ghost);
                int pacmanNode = game.getPacmanCurrentNodeIndex();
                float dis = game.getShortestPathDistance(ghostNode, pacmanNode);

                if (dis < limit) {
                    limit = dis;
                    nearestGhost = ghostNode;
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

        return -2;
    }

}
