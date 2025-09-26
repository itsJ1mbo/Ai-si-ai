package es.ucm.fdi.ici.c2425.practica0.Vicente;

import java.util.EnumMap;
import java.util.Random;

import pacman.controllers.GhostController;
import pacman.game.Constants;
import pacman.game.Constants.GHOST;
import pacman.game.Constants.MOVE;
import pacman.game.Game;

public final class Ghosts extends GhostController {
    private EnumMap<GHOST, MOVE> moves = new EnumMap<GHOST, MOVE>(GHOST.class);
    private MOVE[] allMoves = MOVE.values();
    private Random rnd = new Random();

    @Override
    public EnumMap<GHOST, MOVE> getMove(Game game, long timeDue)
    {
        moves.clear();
        for (GHOST ghostType : GHOST.values())
        {
            if (game.doesGhostRequireAction(ghostType))
            {
                int pacman = game.getPacmanCurrentNodeIndex();
                if(Boolean.TRUE.equals(game.isGhostEdible(ghostType)) || isPacmanCloseToPowerPill(game, 40))
                {
                    moves.put(ghostType, game.getApproximateNextMoveAwayFromTarget(
                            game.getGhostCurrentNodeIndex(ghostType),
                            pacman,
                            game.getGhostLastMoveMade(ghostType),
                            Constants.DM.PATH));
                }
                else
                {
                    if(rnd.nextFloat() < 0.9f)
                        moves.put(ghostType, game.getApproximateNextMoveTowardsTarget(
                                game.getGhostCurrentNodeIndex(ghostType),
                                pacman,
                                game.getGhostLastMoveMade(ghostType),
                                Constants.DM.PATH));
                    else
                        moves.put(ghostType, allMoves[rnd.nextInt(allMoves.length)]);
                }
            }
        }
        return moves;
    }
    
    public String getName() {
    	return "Ghosts";
    }

    private boolean isPacmanCloseToPowerPill(Game game, float limit)
    {
        int[] pills = game.getActivePowerPillsIndices();
        if (pills.length == 0) return false;

        int pacman = game.getPacmanCurrentNodeIndex();

        int i = 0;
        while(i < pills.length
                && game.getShortestPathDistance(pills[i], pacman) > limit)
        {
            i++;
        }
        if (i == pills.length) i--;

        return game.getShortestPathDistance(pills[i], pacman) <= limit;
    }
}
