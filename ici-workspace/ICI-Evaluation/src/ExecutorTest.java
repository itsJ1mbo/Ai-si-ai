import pacman.Executor;
import pacman.controllers.GhostController;
//import pacman.controllers.HumanController;
//import pacman.controllers.KeyBoardInput;
import pacman.controllers.PacmanController;
import es.ucm.fdi.ici.c2425.practica1.grupo08.Ghosts;

/*	ICI HALL-OF-FAME

--- Best MsPacMan (score) --- 
es.ucm.fdi.ici.c2223.practica1.grupo06.MsPacMan (9958)
es.ucm.fdi.ici.c2122.practica1.grupo10.MsPacMan (7164)
es.ucm.fdi.ici.c2324.practica1.grupo07.MsPacMan (5378)
es.ucm.fdi.ici.c2425.practica1.grupo08.MsPacMan (6933)

es.ucm.fdi.ici.c2223.practica2.grupo02.MsPacMan (7000) 
es.ucm.fdi.ici.c2122.practica2.grupo01.MsPacMan (6517) 
es.ucm.fdi.ici.c2324.practica2.grupo01.MsPacMan (4452) 
es.ucm.fdi.ici.c2425.practica2.grupo08.MsPacMan (5553)


--- Best Ghosts (score) --- 
es.ucm.fdi.ici.c2223.practica1.grupo06.Ghosts (2064)
es.ucm.fdi.ici.c2122.practica1.grupo01.Ghosts (2108)
es.ucm.fdi.ici.c2324.practica1.grupo08.Ghosts (2152)
es.ucm.fdi.ici.c2425.practica1.grupo08.Ghosts (3051)


es.ucm.fdi.ici.c2324.practica2.grupo04.Ghosts (1924)
es.ucm.fdi.ici.c2122.practica2.grupo01.Ghosts (2648)
es.ucm.fdi.ici.c2223.practica2.grupo02.Ghosts (3704)
es.ucm.fdi.ici.c2425.practica2.grupo04.Ghosts (2540)


 */



public class ExecutorTest {

    public static void main(String[] args) {
        Executor executor = new Executor.Builder()
                .setTickLimit(4000)
                .setVisual(true)
                .setScaleFactor(2.5)
                .build();

        //PacmanController pacMan = new pacman.controllers.HumanController.HumanController(new pacman.controllers.HumanController.KeyBoardInput());
        //PacmanController pacMan = new es.ucm.fdi.ici.c2526.practica1.grupoK.MsPacMan();
        //GhostController ghosts = new es.ucm.fdi.ici.c2526.practica1.grupoB.Ghosts();

        PacmanController pacMan = new es.ucm.fdi.ici.c2526.practica1.grupoB.MsPacMan();
        GhostController ghosts = new es.ucm.fdi.ici.c2526.practica1.grupoK.Ghosts();
        
        System.out.println( 
            executor.runGame(pacMan, ghosts, 30) //last parameter defines speed
        );     
    }
	
}
