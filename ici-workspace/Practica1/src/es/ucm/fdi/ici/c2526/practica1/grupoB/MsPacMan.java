package es.ucm.fdi.ici.c2526.practica1.grupoB;

import pacman.controllers.PacmanController;
import pacman.game.Constants;
import pacman.game.Constants.MOVE;
import pacman.game.Game;
import pacman.game.GameView;

import java.awt.*;
import java.util.LinkedList;
import java.util.Queue;

public class MsPacMan extends PacmanController{

    @Override
    public MOVE getMove(Game game, long timeDue)
    {
        int pacman = game.getPacmanCurrentNodeIndex();

        Constants.GHOST ghost = getNearestGhostToNode(Float.MAX_VALUE, pacman, game);
        if (ghost == null) {
            int nearestPill = getNearestPill(game);
            if (nearestPill >= 0) {
                return game.getApproximateNextMoveTowardsTarget(pacman, nearestPill, game.getPacmanLastMoveMade(), Constants.DM.PATH);
            } else {
                return MOVE.NEUTRAL;
            }
        }

        if (!game.isJunction(pacman)) return MOVE.NEUTRAL;

        return getMoveToBestJunction(
                750.0f,
                0.5f,
                1.2f,
                100.0f,
                6.0f,
                1.5f,
                1000.0f,
                2000.0f,
                200.0f,
                pacman,
                game);
    }
    
    public String getName() {
    	return "ADG MsPacMan";
    }

    /**
     * Calcula el mejor movimiento desde la posicion actual de Pac-Man hacia la interseccion
     * mas favorable, basandose en una heuristica que equilibra distancia, amenaza de los
     * fantasmas, numero de pildoras y posibles recompensas.
     *
     * @param ghostThreatWeight             Peso aplicado al factor de amenaza de los fantasmas
     * @param endGameThreatMultiplier       Multiplicador aplicado al peso de amenaza cuando quedan pocas pildoras
     * @param junctionDegreeWeight          Peso que penaliza intersecciones con pocas salidas
     * @param pathPillsWeight               Peso aplicado segun la cantidad de pildoras en el camino
     * @param endGamePillsMultiplier        Multiplicador aplicado al peso de pildoras en el late game
     * @param nearestPillWeight             Peso que recompensa caminos mas cercanos a la pildora mas proxima
     * @param endGameNearestPillMultiplier  Multiplicador aplicado al peso de la pildora mas cercana en el late game
     * @param powerPillPenaltyWeight        Peso que penaliza caminos peligrosos sin power pills
     * @param edibleGhostRewardWeight       Peso que recompensa la cercania de fantasmas comestibles
     *
     * @return El mejor movimiento que Pac-Man deberia realizar en la interseccion actual
     */
    private MOVE getMoveToBestJunction(
            float ghostThreatWeight,
            float endGameThreatMultiplier,
            float junctionDegreeWeight,
            float pathPillsWeight,
            float endGamePillsMultiplier,
            float nearestPillWeight,
            float endGameNearestPillMultiplier,
            float powerPillPenaltyWeight,
            float edibleGhostRewardWeight,
            int pacmanNode,
            Game game)
    {
        float bestScore = Float.MAX_VALUE;
        MOVE bestMove = MOVE.NEUTRAL;

        int chosenJunction = 0;

        // Posibles movimientos de Pac-Man en este cruce
        Constants.MOVE[] moves = game.getPossibleMoves(pacmanNode, game.getPacmanLastMoveMade());
        if (moves == null || moves.length == 0) {
            return MOVE.NEUTRAL;
        }

        int totalPills = game.getNumberOfPills() + game.getNumberOfPowerPills();
        float pillRatio = (float)game.getNumberOfActivePills() / totalPills;
        if (pillRatio < 0.3f) {
            pathPillsWeight *= endGamePillsMultiplier;
            ghostThreatWeight *= endGameThreatMultiplier;
            nearestPillWeight *= endGameNearestPillMultiplier;
        }

        for (MOVE move : moves) {
            // Nodo alcanzado tras mover en esta direccion
            int nextNode = game.getNeighbour(pacmanNode, move);

            // Primer cruce alcanzable desde este movimiento
            int junctionNode = Utils.firstJunctionFrom(nextNode, pacmanNode, game);

            // Fantasma mas cercano a este cruce
            Constants.GHOST nearestGhost = getNearestGhostToNode(Float.MAX_VALUE, junctionNode, game);
            int ghostNode = game.getGhostCurrentNodeIndex(nearestGhost);

            // Distancias relevantes
            float ghostToMoveNodeDist = game.getShortestPathDistance(ghostNode, nextNode, game.getGhostLastMoveMade(nearestGhost));
            float pacmanToJunctionDist = game.getShortestPathDistance(nextNode, junctionNode, move);
            float ghostToJunctionDist = game.getShortestPathDistance(ghostNode, junctionNode);

            // Calculo de amenaza/recompensa por fantasmas
            float ghostThreat = 0;
            float edibleReward = 0;
            float proximityPenalty = 9.0f / (ghostToMoveNodeDist + 0.1f);

            if (game.isGhostEdible(nearestGhost)) {
                int edibleTime = game.getGhostEdibleTime(nearestGhost);
                edibleReward = (edibleGhostRewardWeight * edibleTime) / (ghostToJunctionDist + 0.1f);
            } else {
                ghostThreat = (pacmanToJunctionDist - ghostToJunctionDist) / (ghostToJunctionDist + 0.1f);
                ghostThreat += proximityPenalty;
            }

            // Atributos del cruce
            int junctionDegree = game.getPossibleMoves(junctionNode).length - 1;

            // Pildoras en el camino hacia el cruce
            int[] pathToJunction = game.getShortestPath(nextNode, junctionNode, move);
            int pillsOnPath = countAvailablePillsOnPath(game, pathToJunction);

            // Penalizacion o beneficio por power pills segun situacion
            float powerPillPenalty = 0;
            int nearbyGhosts = nearGhostsNumber(game, 20, pacmanNode);
            if (pathHasPowerPill(pathToJunction, game)) {
                if (nearbyGhosts >= 2) {
                    // Beneficio: hay fantasmas y power pill
                    powerPillPenalty = -powerPillPenaltyWeight * nearbyGhosts;
                }
                else {
                    // Penalizacion: fantasmas cerca sin power pill
                    powerPillPenalty = powerPillPenaltyWeight;
                }
            } else if (nearbyGhosts >= 2) {
                // Penalizacion: fantasmas cerca sin power pill
                powerPillPenalty = powerPillPenaltyWeight * nearbyGhosts;
            }

            // Bonus por pill mas cercana (si el camino no tiene pills)
            float nearestPillBonus = 0;
            if (pillsOnPath == 0) {
                int nearestPillNode = getNearestPill(game);
                if (nearestPillNode >= 0) {
                    float dist = game.getShortestPathDistance(junctionNode, nearestPillNode);
                    nearestPillBonus = nearestPillWeight / (dist + 0.1f);
                }
            }

            // Heuristica final
            float score = pacmanToJunctionDist
                    + ghostThreatWeight * ghostThreat
                    - junctionDegreeWeight * junctionDegree
                    - pathPillsWeight * pillsOnPath
                    - edibleReward
                    - nearestPillBonus
                    + powerPillPenalty;

            // Si la heuristica es mejor, actualizamos
            if (score <= bestScore) {
                bestScore = score;
                bestMove = move;
                chosenJunction = junctionNode;
            }
        }

        // Visualizacion en pantalla
        GameView.addPoints(game, Color.yellow,
                game.getShortestPath(chosenJunction, game.getPacmanCurrentNodeIndex()));

        return bestMove;
    }

    /**
     * Cuenta las pills que hay en una camino
     *
     * @param path El camino de nodos en el que busca
     *
     * @return El numero de pills en el camino
     */
    private int countAvailablePillsOnPath(Game game, int[] path) {
        if (path == null || path.length == 0) return 0;

        int count = 0;
        for (int node : path) {
            int pillIndex = game.getPillIndex(node);
            if (pillIndex != -1 && Boolean.TRUE.equals(game.isPillStillAvailable(pillIndex))) count++;
            int powerPillIndex = game.getPowerPillIndex(node);
            if (powerPillIndex != -1 && Boolean.TRUE.equals(game.isPowerPillStillAvailable(powerPillIndex))) count++;
        }

        return count;
    }

    /**
     * Comprueba si en el camino dado hay una power pill
     *
     * @param path Camino en el que busca una power pill
     *
     * @return True si encuentra, false en caso contrario
     */
    private boolean pathHasPowerPill(int[] path, Game game) {
        int i = 0;
        while (i < path.length) {
            int powerPillIndex = game.getPowerPillIndex(path[i]);
            if (powerPillIndex != -1 && Boolean.TRUE.equals(game.isPowerPillStillAvailable(powerPillIndex)))
                return true;

            i++;
        }

        return false;
    }

    /**
     * Busca el fantasma mas cercano a un nodo dado segun un limite
     *
     * @param limit Limite a partir del que encuentra
     * @param n El nodo
     *
     * @return El fantasma mas cercano
     */
    private Constants.GHOST getNearestGhostToNode(float limit, int n, Game game) {
        Constants.GHOST nearestGhost = null;

        for(Constants.GHOST ghost : Constants.GHOST.values()) {
            if(game.getGhostLairTime(ghost) <= 0) {
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

    /**
     * Busca la pill mas cercana al pacman
     *
     * @return El id del nodo de la pill
     */
    private int getNearestPill(Game game) {
        int pacman = game.getPacmanCurrentNodeIndex();

        Queue<Integer> queue = new LinkedList<>();
        boolean[] visited = new boolean[game.getNumberOfNodes()];

        queue.add(pacman);
        visited[pacman] = true;

        while (!queue.isEmpty()) {
            int current = queue.poll();

            int pillIndex = game.getPillIndex(current);
            int powerPillIndex =  game.getPowerPillIndex(current);
            if((pillIndex != -1 && Boolean.TRUE.equals(game.isPillStillAvailable(pillIndex))) ||
                    (powerPillIndex != -1 && Boolean.TRUE.equals(game.isPowerPillStillAvailable(powerPillIndex))))
                return current;

            int[] neighbours = game.getNeighbouringNodes(current);
            for (int neighbour : neighbours) {
                if (!visited[neighbour]) {
                    visited[neighbour] = true;
                    queue.add(neighbour);
                }
            }
        }

        return -2;
    }

    /**
     * Busca la cantidad de fantasmas cercanos al pacman
     *
     * @param limit Limite a partir del que encuentra fantasmas
     * @param pacman Nodo del pacman
     *
     * @return Cantidad de fantasmas cercanos
     */
    private int nearGhostsNumber(Game game, int limit, int pacman) {
        int closeGhosts = 0;

        for (Constants.GHOST g : Constants.GHOST.values()) {
            if (!game.isGhostEdible(g) && game.getGhostLairTime(g) == 0) {
                int ghostNode = game.getGhostCurrentNodeIndex(g);
                int dist = game.getShortestPathDistance(pacman, ghostNode);
                if (dist > 0 && dist <= limit) {
                    closeGhosts++;
                }
            }
        }

        return closeGhosts;
    }

}
