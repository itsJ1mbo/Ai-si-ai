package es.ucm.fdi.ici.c2526.practica1.grupoYY;

import pacman.controllers.PacmanController;
import pacman.game.Constants;
import pacman.game.Constants.MOVE;
import pacman.game.Game;
import pacman.game.GameView;
import pacman.game.internal.Ghost;

import java.awt.*;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

public class MsPacMan extends PacmanController{

    @Override
    public MOVE getMove(Game game, long timeDue)
    {
        int pacman = game.getPacmanCurrentNodeIndex();

        Constants.GHOST ghost = getNearestGhostToNode(Float.MAX_VALUE, pacman, game);
        if (ghost == null)
        {
            int nearestPill = getNearestPill(game);
            if (nearestPill >= 0)
            {
                return game.getApproximateNextMoveTowardsTarget(pacman, nearestPill, game.getPacmanLastMoveMade(), Constants.DM.PATH);
            } else
            {
                return MOVE.NEUTRAL;
            }
        }

        if (!game.isJunction(pacman)) return MOVE.NEUTRAL;

        return getMoveToBestJunction(1.2f, 0.5f, 0.9f, 0.8f, pacman, game);
    }
    
    public String getName() {
    	return "MsPacManNeutral";
    }

    private MOVE getMoveToBestJunction(float alpha, float beta, float gamma, float delta, int pacman, Game game)
    {
        float max = Float.MAX_VALUE;
        int c = 0;
        int f = 0;
        MOVE bestMove = MOVE.NEUTRAL;

        Constants.MOVE[] moves = game.getPossibleMoves(pacman, game.getPacmanLastMoveMade());
        if (moves == null || moves.length == 0) return MOVE.NEUTRAL;
        for (MOVE move : moves)
        {
            int moveNode = game.getNeighbour(pacman, move);
            int possibleJunctionNode = firstJunctionFrom(moveNode, pacman, game);

            Constants.GHOST ghost = getNearestGhostToNode(Float.MAX_VALUE, possibleJunctionNode, game);
            int ghostToJunctionNode = game.getGhostCurrentNodeIndex(ghost);

            float gm = game.getShortestPathDistance(ghostToJunctionNode, moveNode, game.getGhostLastMoveMade(ghost));

            float gc = game.getShortestPathDistance(ghostToJunctionNode, possibleJunctionNode);
            if(game.isGhostEdible(ghost))
            {
                gc *= -0.1f * ((4 % edibleGhosts(game)) + 1);
            }
            float threat = 1 / (gc + 0.1f);

            float d = game.getShortestPathDistance(pacman, possibleJunctionNode);

            int degree = game.getPossibleMoves(possibleJunctionNode).length - 1;

            int[] path = game.getShortestPath(pacman, possibleJunctionNode);
            int pillsOnPath = countAvailablePillsOnPath(game, path);

            float score = (d / gc) * 8.0f
                    + alpha * threat
                    - beta * degree
                    - gamma * pillsOnPath
                    + 1.0f / (gm + 0.1f);

            if (pillsOnPath == 0)
            {
                int nearestPill = getNearestPill(game);
                float dist = game.getShortestPathDistance(possibleJunctionNode, nearestPill);
                score -= delta / (dist + 0.1f);
            }

            if (score <= max)
            {
                max = score;
                c = possibleJunctionNode;
                f = ghostToJunctionNode;
                bestMove = move;
            }
        }

        GameView.addPoints(
                game,
                Color.yellow,
                game.getShortestPath(
                        c,
                        game.getPacmanCurrentNodeIndex()));

        GameView.addPoints(
                game,
                Color.red,
                game.getShortestPath(
                        f,
                        c));

        return bestMove;
    }

    private int edibleGhosts(Game game)
    {
        int count = 0;
        for (Constants.GHOST ghost : Constants.GHOST.values())
        {
            if(game.isGhostEdible(ghost)) count++;
        }
        return count;
    }

    private int countAvailablePillsOnPath(Game game, int[] path)
    {
        if (path == null || path.length == 0) return 0;

        int count = 0;
        for (int node : path)
        {
            int pillIndex = game.getPillIndex(node);
            if (pillIndex != -1 && Boolean.TRUE.equals(game.isPillStillAvailable(pillIndex))) count++;
            int powerPillIndex = game.getPowerPillIndex(node);
            if (powerPillIndex != -1 && Boolean.TRUE.equals(game.isPowerPillStillAvailable(powerPillIndex))) count++;
        }

        return count;
    }

    private int firstJunctionFrom(int node, int parent, Game game)
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
                if (n != prev)
                {
                    next = n;
                    break;
                }
            }

            prev = current;
            current = next;

            steps++;
        }

        return -2;
    }


    private Constants.GHOST getNearestGhostToNode(float limit, int n, Game game)
    {
        Constants.GHOST nearestGhost = null;

        for(Constants.GHOST ghost : Constants.GHOST.values())
        {
            if(game.getGhostLairTime(ghost) <= 0)
            {
                int ghostNode = game.getGhostCurrentNodeIndex(ghost);
                float dis = game.getShortestPathDistance(ghostNode, n);

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

        return -2;
    }

}
