package es.ucm.fdi.ici.c2526.practica1.grupoYY;

import java.awt.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.LinkedList;

import pacman.controllers.GhostController;
import pacman.game.Constants;
import pacman.game.Constants.GHOST;
import pacman.game.Constants.MOVE;
import pacman.game.Game;
import pacman.game.GameView;

public final class Ghosts extends GhostController {
    private EnumMap<GHOST, MOVE> moves = new EnumMap<GHOST, MOVE>(GHOST.class);

    enum Roles {
        first_intersection,
        second_intersection,
        third_intersection
    }

    Roles[] ghostRoles = new Roles[] {Roles.first_intersection, Roles.second_intersection,
            Roles.first_intersection, Roles.second_intersection};

    int[] roles_count = new int[] {2, 1, 0};
    int[] followed_ghosts = new int[] {0, 0, 0, 0};
    Color[] ghostColors = new Color[] {Color.RED, Color.PINK, Color.BLUE, Color.ORANGE};

    int threshold_1 = 20;
    int threshold_2 = 40;
    // TODO: alejarse del pacman y de los fantasmas comestibles si eres comestible
    // TODO: acercarse a los fantasmas no comestibles o al spawn

    @Override
    public EnumMap<GHOST, MOVE> getMove(Game game, long timeDue) {
        moves.clear();
        for (GHOST ghost : GHOST.values()) {
            int target = game.getPacmanCurrentNodeIndex();
            if (game.doesGhostRequireAction(ghost)) {

                int pacman = game.getPacmanCurrentNodeIndex();

                if(Boolean.FALSE.equals(game.isGhostEdible(ghost))){
                    // cambio de roles
                    if(game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), pacman, game.getGhostLastMoveMade(ghost)) <= 20
                    && (roles_count[Roles.first_intersection.ordinal()] < 2)) {
                        changeRole(game, ghost, Roles.first_intersection); }
                    else if(game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), pacman, game.getGhostLastMoveMade(ghost)) <= 20
                    && (roles_count[Roles.second_intersection.ordinal()] < 2)) {
                        changeRole(game, ghost, Roles.second_intersection); }
                    else if((roles_count[Roles.third_intersection.ordinal()] < 2))
                        changeRole(game, ghost, Roles.third_intersection);

                    int depth = 4; int type = 0;
                    if(ghostRoles[ghost.ordinal()] == Roles.first_intersection) { type = 1; }
                    else if(ghostRoles[ghost.ordinal()] == Roles.second_intersection) { type = 2;}
                    else if(ghostRoles[ghost.ordinal()] == Roles.third_intersection){ type = 3;}

                    int g = game.getGhostCurrentNodeIndex(ghost);
                    ArrayList<ArrayList<Integer>> junctions = new ArrayList<ArrayList<Integer>>();
                    MOVE lastMove = game.getPacmanLastMoveMade();
                    int result = predictPacmanTarget(game, depth, 0, pacman, lastMove,  junctions);
                    int best_junction = getBestJunction(game, junctions, type, ghost);
                    if(best_junction > -1){ target = best_junction; }

                    MOVE m = game.getApproximateNextMoveTowardsTarget(g, target,
                            game.getGhostLastMoveMade(ghost), Constants.DM.PATH);
                    moves.put(ghost, m); }
                else {
                    // busca los fantasmas vivos, si no hay, busca el fantasma comestible mas cercano y se aleja,
                    // si no hay, se aleja del pacman
                    // valor default
                    MOVE away = game.getNextMoveAwayFromTarget(game.getGhostCurrentNodeIndex(ghost),
                            game.getPacmanCurrentNodeIndex(), Constants.DM.PATH);

                    Integer edible_ghosts = 0;

                    GHOST g = getClosestNonEdibleGhost(game, ghost, edible_ghosts);
                    if(g == null) {
                        g = getClosestEdibleGhost(game, ghost); }

                    // si el fantasma encontrado NO es comestible, va hacia el
                    if(Boolean.FALSE.equals(game.isGhostEdible(g))) {
                        away = game.getNextMoveTowardsTarget(game.getGhostCurrentNodeIndex(ghost),
                                game.getGhostCurrentNodeIndex(g), Constants.DM.PATH); }
                    else{
                        game.getNextMoveAwayFromTarget(game.getGhostCurrentNodeIndex(ghost),
                                game.getGhostCurrentNodeIndex(g), Constants.DM.PATH); }

                    moves.put(ghost, away);
                }
            }

            if(game.getGhostLairTime(ghost) <= 0) {
                GameView.addPoints(
                        game,
                        ghostColors[ghost.ordinal()],
                        game.getShortestPath(
                                game.getGhostCurrentNodeIndex(ghost),
                                target));
            }

        }
        return moves;
    }

    private GHOST getClosestNonEdibleGhost(Game game, GHOST this_ghost, Integer edible_count) {
        int distance = Integer.MAX_VALUE; GHOST g = null;
        for (GHOST ghost : GHOST.values()){
            if(this_ghost != ghost && Boolean.FALSE.equals(game.isGhostEdible(ghost))) {
                int d = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(this_ghost), game.getGhostCurrentNodeIndex(ghost));
                if(d < distance) { distance = d; g = ghost; }
            } else { edible_count++; }
        }
        return g;
    }

    private GHOST getClosestEdibleGhost(Game game, GHOST this_ghost){
        int distance = Integer.MAX_VALUE;
        GHOST g = null;
        for (GHOST ghost : GHOST.values()){
            if(this_ghost != ghost && Boolean.TRUE.equals(game.isGhostEdible(ghost))) {
                int d = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(this_ghost), game.getGhostCurrentNodeIndex(ghost));
                if( d < distance) { distance = d; g = ghost; }
            }
        }
        return g;

    }

    private void changeRole(Game game, GHOST ghost, Roles role){
        roles_count[ghostRoles[ghost.ordinal()].ordinal()]--;
        ghostRoles[ghost.ordinal()] = role;
        roles_count[role.ordinal()] +=1; }

    private int predictPacmanTarget(Game game, int depth, int count, int initialNode, MOVE lastMove,
                                    ArrayList<ArrayList<Integer>> junctions) {
        if(count >= depth-1){ return 0; }
        if(initialNode == -1){ return 0; }

        MOVE[] moves = game.getPossibleMoves(initialNode, lastMove);
        ArrayList<Integer> list = new ArrayList<Integer>();
        ArrayList<MOVE> move_list = new ArrayList<MOVE>();

        // -------------------------- TARGET ---------------------------------
        // recorre los movimientos posibles para encontrar la interseccion final
        for(MOVE move : moves){
            int junct = firstJunctionFrom(game.getNeighbour(initialNode, move), initialNode, game);
                    //getNextJunction(game, current, move);
            list.add(junct);
            move_list.add(move);

            int a = predictPacmanTarget(game, depth, count + 1,
                    junct, move, junctions);
        }
        junctions.add(list);

        return 0;
    }

    private int getBestJunction(Game game, ArrayList<ArrayList<Integer>> junctions,
                                int depth, GHOST ghost){
        int junct = 0; int shortest_dist = 0;

        // si el nivel de profundiad es demasiado se queda solo con el maximo
        if(depth > junctions.size()){ depth = junctions.size(); }

        ArrayList<Integer> list = junctions.get(junctions.size() - depth);
        if(list.isEmpty()){ return -1; }

        junct = list.getFirst();
        shortest_dist = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), junct);

        for(int i = 1; i<list.size(); i++){
            int dist = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), list.get(i));

            if(dist < shortest_dist){
                shortest_dist = dist;
                junct = list.get(i); }
        }
        return junct;
    }

    private int firstJunctionFrom(int node, int parent, Game game)
    {
        int current = node; int prev = parent; int steps = 0; int maxSteps = game.getNumberOfNodes();

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

    public String getName() {
    	return "GhostsNeutral";
    }
}
