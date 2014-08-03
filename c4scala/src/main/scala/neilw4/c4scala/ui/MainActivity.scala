package neilw4.c4scala.ui

import android.app.Activity
import android.os.Bundle
import android.view.ViewGroup.LayoutParams
import android.view.{Menu, MenuItem, View}
import android.widget.AdapterView.OnItemClickListener
import android.widget._
import neilw4.c4scala.controller.Controller
import neilw4.c4scala.state._
import neilw4.c4scala.R

trait UiCallback {
    def onColumnSelected(column: Int)
}

class MainActivity extends Activity with StateListener with BoardSizeSetter {

    var state: State = null
    var callback: UiCallback = null

    var vDifficultySeekBar: SeekBar = null
    var vDifficultyText: TextView = null
    var vPlayerAAiToggle: MenuItem = null
    var vPlayerBAiToggle: MenuItem = null
    var vBoardGridContainer: View = null
    var vBoardGrid: GridView = null
    var vBoardGridAdapter: BoardAdapter = null

    override def onCreate(savedInstanceState: Bundle) = {
        super.onCreate(savedInstanceState)
        state = savedInstanceState match {
            case null => new State
            case _ => savedInstanceState.getParcelable[State](State.KEY) match {
                case state: State => state
                case _ => new State
            }
        }
        state.attachListener(this)
        callback = new Controller(state)

        setContentView(R.layout.activity_main)

        vDifficultySeekBar = findViewById(R.id.difficulty_seekbar).asInstanceOf[SeekBar]
        vDifficultySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener {
            override def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) = if (fromUser) state.setDifficulty(progress + 1)
            override def onStartTrackingTouch(seekBar: SeekBar) = {}
            override def onStopTrackingTouch(seekBar: SeekBar) = {}
        })
        vDifficultyText = findViewById(R.id.difficulty_text).asInstanceOf[TextView]

        vBoardGridContainer = findViewById(R.id.board_grid_container)
        vBoardGrid = findViewById(R.id.board_grid).asInstanceOf[GridView]
        vBoardGridAdapter = new BoardAdapter(MainActivity.this, state, vBoardGridContainer, this)
        vBoardGrid.setNumColumns(Board.WIDTH)
        vBoardGrid.setAdapter(vBoardGridAdapter)
        vBoardGrid.setOnItemClickListener(new OnItemClickListener {
            override def onItemClick(parent: AdapterView[_], View: View, position: Int, id: Long): Unit = {
                val col: Int = parent.getAdapter.asInstanceOf[BoardAdapter].column(position)
                callback.onColumnSelected(col)
            }
        })
        state.callAllListeners
    }

    override def onDestroy() = {
      super.onDestroy()
      state.removeListener(this)
    }

    override def onSaveInstanceState(savedInstanceState: Bundle) = savedInstanceState.putParcelable(State.KEY, state)

    override def onCreateOptionsMenu(menu: Menu) = {
        getMenuInflater.inflate(R.menu.main, menu)
        vPlayerAAiToggle = menu.findItem(R.id.player_A_toggle)
        vPlayerBAiToggle = menu.findItem(R.id.player_B_toggle)
        state.playerAi.keys.foreach(piece => onPlayerAiChanged(piece, state.playerAi(piece)))
        true
    }

    override def onOptionsItemSelected(item: MenuItem): Boolean = {
        item.getItemId match {
            case R.id.new_game => state.newGame
            case R.id.player_A_toggle => state.setPlayerAi(YELLOW, !state.playerAi(YELLOW))
            case R.id.player_B_toggle => state.setPlayerAi(RED, !state.playerAi(RED))
        }
        true
    }

    override def onDifficultyChanged(difficulty: Int) = {
        vDifficultySeekBar.setProgress(difficulty)
        vDifficultyText.setText("Difficulty: " + difficulty)
    }

    override def onPlayerAiChanged(piece: Piece, isAi: Boolean) = piece match {
        case YELLOW => if (vPlayerAAiToggle != null) vPlayerAAiToggle.setIcon(isAi match {
            case true => R.drawable.yellow_ai
            case false => R.drawable.yellow_user
        })
        case RED => if (vPlayerBAiToggle != null) vPlayerBAiToggle.setIcon(isAi match {
            case true => R.drawable.red_ai
            case false => R.drawable.red_user
        })
    }

    override def setBoardSize(width: Int, height: Int) = {
        var layout: LayoutParams = vBoardGrid.getLayoutParams
        if (layout == null) {
            layout = new RelativeLayout.LayoutParams(width, height)
        } else {
            layout.height = height
            layout.width = width
        }
        vBoardGrid.setLayoutParams(layout)
    }

    override def onBoardPieceChanged(x: Int, y: Int) = vBoardGridAdapter.notifyDataSetChanged()

    override def onGameEnd(winner: Piece) = {
        val message = getResources.getString(winner.win_text)
        Toast.makeText(getBaseContext, message, Toast.LENGTH_LONG).show()
    }
}
