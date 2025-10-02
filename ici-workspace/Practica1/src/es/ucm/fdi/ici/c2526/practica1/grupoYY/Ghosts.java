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
        third_intersection,
        run_away
    }

    Roles[] ghostRoles = new Roles[] {Roles.first_intersection, Roles.second_intersection,
            Roles.first_intersection, Roles.second_intersection};

    int[] roles_count = new int[] {2, 1, 0};

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

                    game.getPacmanLastMoveMade();
                    int g = game.getGhostCurrentNodeIndex(ghost);
                    //
                    int moveNode = game.getNeighbour(pacman, game.getPacmanLastMoveMade());

                    ArrayList<ArrayList<Integer>> junctions = new ArrayList<ArrayList<Integer>>();
                    MOVE lastMove = game.getPacmanLastMoveMade();

                    int pm = game.getPacmanCurrentNodeIndex();
                    int junction = predictPacmanTarget(game, depth, 0, pm, lastMove,  junctions);
                    int best_junction = getBestJunction(game, junctions, type, ghost);

                    if(best_junction > -1){ target = best_junction; }

                    MOVE m = game.getApproximateNextMoveTowardsTarget(g, target,
                            game.getGhostLastMoveMade(ghost), Constants.DM.PATH);

                    moves.put(ghost, m); }
                else {
                    // si es comestible le cambia el rol, aun que probablemente no haga falta


                    // de momento solo se aleja del pacman
                    MOVE away = game.getNextMoveAwayFromTarget(game.getGhostCurrentNodeIndex(ghost),
                                game.getPacmanCurrentNodeIndex(), Constants.DM.PATH);


                    // tener en cuenta tmb que se aleje de otros fantasmas comestibles y se acerque a los
                    // no comestibles

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

    private void changeRole(Game game, GHOST ghost, Roles role){
        roles_count[ghostRoles[ghost.ordinal()].ordinal()]--;
        ghostRoles[ghost.ordinal()] = role;
        roles_count[role.ordinal()] +=1;
    }

    // es el metodo del jimbo methinks pero es que no me va asi que lol lmao
    // pone en orden (¿?) las posiciones a las que cree que puede ir
    private int predictPacmanTarget(Game game, int depth, int count, int initialNode, MOVE lastMove,
                                    ArrayList<ArrayList<Integer>> junctions){

        if(count >= depth-1){
            return 0;
        }
        if(initialNode == -1){
            return 0;
        }

        boolean aux = game.isJunction(initialNode);

        MOVE[] moves = game.getPossibleMoves(initialNode, lastMove);

        int current = initialNode;
        int prev = current;

        int j = 0;

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



        // recorre todas las intersecciones de la profundidad count
//        for(int i = 0; i<junctions.get(count).size(); i++){
//            int a = predictPacmanTarget(game, depth, count + 1,
//                    junctions.get(count).get(i), move_list.get(i), junctions);
//
//        }

//
//        if(count+1 < depth && current > -1){
//            for(int i = 0; i<junctions.get(count).size(); i++){
//                MOVE move = move_list.get(i);
//                int c = count + 1;
//                // junctions.get(count).get(i)
//                int a = predictPacmanTarget(game, depth, c, current, move, junctions);
//            }
//        }



//        // recursividad segun la profundiad (cuantas junctions hacia delante quieres 'predecir')
//        if(count+1 < depth){
//            MOVE[] moves_rec = game.getPossibleMoves(pm, game.getPacmanLastMoveMade());
//            int [] targets_rec = new int[moves_rec.length];
//            int i = 0;
//            while (i<moves_rec.length){
//                targets_rec[i] = predictPacmanTarget(game, depth, count++, junctions);
//                i++;
//            }
//        }

        return 0;
    }


    private int getNextJunction(Game game, int init, MOVE move)
    {
        int steps = 0;
        int maxSteps = game.getNumberOfNodes();

        int current = init;
        int prev = init;
        boolean found = false;

        while(steps < maxSteps && !found){

            // si el nodo es vaildo
            if(current > -1) {
                // si es una interseccion
                if (game.isJunction(current)) {
                    // cada posible direccion del pacman es una posible solucion del fantasma
                    found = true;
                }
                else {
                    // siguiente nodo en esa direccion/movimiento
                    int next = game.getNeighbour(current, move);
                    prev = current;
                    current = next;
                    steps++;
                }
            }
            else {
                // cada posible direccion del pacman es una posible solucion del fantasma
                found = true;
            }
        }
        return current;
    }

    // puaj
    // LKWFHLKSUDBFÑLSDHFSÑLEF AQUI
    private int getBestJunction(Game game, ArrayList<ArrayList<Integer>> junctions,
                                int depth, GHOST ghost){
        int junct = 0;
        int shortest_dist = 0;

        // si el nivel de profundiad es demasiado se queda solo con el maximo
        if(depth > junctions.size()){
            depth = junctions.size();
            //return game.getPacmanCurrentNodeIndex();
        }

        ArrayList<Integer> list = junctions.get(junctions.size() - depth);

        if(list.size() == 0){
            return -1;
        }
        junct = list.getFirst();
        shortest_dist = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), junct);

        for(int i = 1; i<list.size(); i++){
            int dist = game.getShortestPathDistance(game.getGhostCurrentNodeIndex(ghost), list.get(i));

            if(dist < shortest_dist){
                shortest_dist = dist;
                junct = list.get(i);
            }
        }

        return junct;
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



    public String getName() {
    	return "GhostsNeutral";
    }
}
