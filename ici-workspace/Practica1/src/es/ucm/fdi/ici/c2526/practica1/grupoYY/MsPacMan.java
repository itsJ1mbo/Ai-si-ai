package es.ucm.fdi.ici.c2526.practica1.grupoYY;

import pacman.controllers.PacmanController;
import pacman.game.Constants.MOVE;
import pacman.game.Game;

public class MsPacMan extends PacmanController{

    @Override
    public MOVE getMove(Game game, long timeDue) {
        return MOVE.NEUTRAL;
    }
    
    public String getName() {
    	return "MsPacManNeutral";
    }

}
