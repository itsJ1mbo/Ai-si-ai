package es.ucm.fdi.ici.c2526.practica1.grupoYY;

import java.awt.*;
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
        run_away
    }

    Roles[] ghostRoles = new Roles[] {Roles.first_intersection, Roles.first_intersection,
            Roles.first_intersection, Roles.first_intersection};

    Color[] ghostColors = new Color[] {Color.RED, Color.PINK, Color.BLUE, Color.ORANGE};

    // TODO: calcular la interseccion a la que va el pacman
    // TODO: intentrar predecir la siguiente interseccion a la que va a ir (rol 2)
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
                    if(ghostRoles[ghost.ordinal()] == Roles.first_intersection) {
                        game.getPacmanLastMoveMade();

                        int g = game.getGhostCurrentNodeIndex(ghost);
                        //
                        int moveNode = game.getNeighbour(pacman, game.getPacmanLastMoveMade());


                        // esto no me gusta lol
                        //int[] junctions = new int[0];
                        ArrayList<Integer> junctions = new ArrayList<Integer>();
                        int junction = predictPacmanTarget(game, 2, 0, junctions);
                        //target = junction;
                        //int sp = game.getShortestPathDistance();

                        MOVE m = game.getApproximateNextMoveTowardsTarget(g, junction,
                                game.getGhostLastMoveMade(ghost), Constants.DM.PATH);

                        moves.put(ghost, m);
                    }
                    else{
                        moves.put(ghost, MOVE.NEUTRAL);

                    }

                }
                else {
                    // si es comestible le cambia el rol, aun que probablemente no haga falta
                    if(ghostRoles[ghost.ordinal()] != Roles.run_away) {
                        ghostRoles[ghost.ordinal()] = Roles.run_away;
                    }

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

    // es el metodo del jimbo methinks pero es que no me va asi que lol lmao
    // pone en orden (¿?) las posiciones a las que cree que puede ir
    private int predictPacmanTarget(Game game, int depth, int count, ArrayList<Integer> junctions){

        int pm = game.getPacmanCurrentNodeIndex();

        MOVE[] moves = game.getPossibleMoves(pm, game.getPacmanLastMoveMade());

        int current = pm;
        int prev = current;

        int j = 0;

        // -------------------------- TARGET ---------------------------------
        // recorre los movimientos posibles para encontrar la interseccion final
        for(MOVE move : moves){

            int steps = 0;
            int maxSteps = game.getNumberOfNodes();

            boolean found = false;
            while(steps < maxSteps && !found){

                // si el nodo es vaildo
                if(current > -1) {
                    // si es una interseccion
                    if (game.isJunction(current)) {
                        // cada posible direccion del pacman es una posible solucion del fantasma
                        junctions.add(current);
                        j++;
                        found = true;
                    }

                    // siguiente nodo en esa direccion/movimiento
                    int next = game.getNeighbour(current, move);
                    prev = current;
                    current = next;
                    steps++;
                }
                else {
                    // cada posible direccion del pacman es una posible solucion del fantasma
                    junctions.add(prev);
                    j++;
                    found = true;
                }
            }


            // recursividad aqui
            int b = 0;

            if(count+1 < depth){
                int a = predictPacmanTarget(game, depth, ++count, junctions);
            }
        }

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

        return junctions.get(0);
    }


    // puaj
    private int possibleMovesWithDepth(Game game, int depth, int initial){
        int count = 0;
        int i = 0;

        while(i < depth){


            i++;
        }


        return count;
    }
    // guarrada de placeholder
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

        int i = 0;

        return -2;
    }

    public String getName() {
    	return "GhostsNeutral";
    }
}
