package es.ucm.fdi.ici.c2526.practica1.grupoB;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;

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

    int threshold_1 = 10;
    int threshold_2 = 20;

    @Override
    public EnumMap<GHOST, MOVE> getMove(Game game, long timeDue) {
        moves.clear();
        for (GHOST ghost : GHOST.values()) {
            int target = game.getPacmanCurrentNodeIndex();
            if (game.doesGhostRequireAction(ghost)) {

                int pacman = game.getPacmanCurrentNodeIndex();

                if(Boolean.FALSE.equals(game.isGhostEdible(ghost))){
                    // cambio de roles
                    if(game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), pacman, game.getGhostLastMoveMade(ghost)) <= threshold_1
                    && (roles_count[Roles.first_intersection.ordinal()] < 2)) {
                        changeRole(game, ghost, Roles.first_intersection); }
                    else if(game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), pacman, game.getGhostLastMoveMade(ghost)) <= threshold_2
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

                    // busca si hay algun fantasma comestible
                    GHOST closest_edible_ghost = null;
                    int closest_dist = Integer.MAX_VALUE;
                    for(GHOST is_g_edible : GHOST.values()){
                        if(Boolean.TRUE.equals(game.isGhostEdible(is_g_edible))) {
                            int aux_dist = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost),
                                    game.getGhostCurrentNodeIndex(is_g_edible),
                                    game.getGhostLastMoveMade(ghost));
                            if(aux_dist < closest_dist){
                                closest_dist = aux_dist;
                                closest_edible_ghost = is_g_edible;
                            }
                        }
                    }
                    // resetta el array
                    if(closest_edible_ghost == null){ Arrays.fill(followed_ghosts, 0); }

                    if(closest_edible_ghost != null && followed_ghosts[closest_edible_ghost.ordinal()] == 0){
                        followed_ghosts[closest_edible_ghost.ordinal()] = 1;
                        target = game.getGhostCurrentNodeIndex(closest_edible_ghost);
                    }
                    else {
                        int result = predictPacmanTarget(game, depth, 0, pacman, lastMove,  junctions);
                        int best_junction = getBestJunction(game, junctions, type, ghost);
                        if(best_junction > -1) { target = best_junction; }
                    }

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
                        away = game.getNextMoveAwayFromTarget(game.getGhostCurrentNodeIndex(ghost),
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
                                target));}

        }
        return moves;
    }

    /**
     *
     * @param game          referencia al juego
     * @param this_ghost    el fantasma desde el que estamos calculando
     * @param edible_count  contador con fantasmas comestibles
     * @return              el fantasma mas cercano no comestible, si no hay devuelve null
     */
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

    /**
     *
     * @param game          referencia al game
     * @param this_ghost    el fantasma desde el que estamos calculando
     * @return              el fantasma mas cercano comestible, si no hay devuelve null
     */
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

    /**
     *
     * @param game          referencia al juego
     * @param ghost         fantasma que quieras cambiar de rol
     * @param role          el nuevo rol
     */
    private void changeRole(Game game, GHOST ghost, Roles role){
        roles_count[ghostRoles[ghost.ordinal()].ordinal()]--;
        ghostRoles[ghost.ordinal()] = role;
        roles_count[role.ordinal()] +=1; }

    /**
     *
     * @param game          referencia al juego
     * @param depth         la 'profundiad' de la busqueda (cuantas intersecciones hacia delante quieres mirar)
     * @param count         contador de la interseccion por la que va, si es el primero poned 0
     * @param initialNode   nodo inicial desde el que se quiere calcular
     * @param lastMove      ultimo movimiento del bicho (no tiene por que ser el pacman)
     * @param junctions     arrayList de arrayList que pasa los nodos de las posibles intersecciones segun la profundidad
     * @return
     */
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
            int junct = Utils.firstJunctionFrom(game.getNeighbour(initialNode, move), initialNode, game);
                    //getNextJunction(game, current, move);
            list.add(junct);
            move_list.add(move);

            int a = predictPacmanTarget(game, depth, count + 1,
                    junct, move, junctions);
        }
        junctions.add(list);

        return 0;
    }

    /**
     *
     * @param game          referencia al juego
     * @param junctions     arrayList de arrayList con las intersecciones por profundidad
     * @param depth         profundidad a la que se quiera ir
     * @param ghost         fantasma desde el que estamos calculando
     * @return
     */
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


    public String getName() {
    	return "ADG Ghosts";
    }
}
