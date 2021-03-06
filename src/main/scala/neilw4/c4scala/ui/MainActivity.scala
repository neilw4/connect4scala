package neilw4.c4scala.ui

import android.app.Activity
import android.graphics.{PorterDuffColorFilter, PorterDuff}
import android.os.Bundle
import android.view.ViewGroup.LayoutParams
import android.view.{Menu, MenuItem, View}
import android.widget.AdapterView.OnItemClickListener
import android.widget._
import neilw4.c4scala.controller.Controller
import neilw4.c4scala.state._
import neilw4.c4scala.R

class MainActivity extends Activity with StateListener with BoardSizeSetter {

    var state: State = null
    var controller: Controller = null

    lazy val vDifficultySeekBar: SeekBar = findViewById(R.id.difficulty_seekbar).asInstanceOf[SeekBar]
    lazy val vThinkingIndicator: ProgressBar = findViewById(R.id.thinking_indicator).asInstanceOf[ProgressBar]
    lazy val vDifficultyText: TextView = findViewById(R.id.difficulty_text).asInstanceOf[TextView]
    var vPlayerAAiToggle: MenuItem = null
    var vPlayerBAiToggle: MenuItem = null
    lazy val vBoardGrid: GridView = findViewById(R.id.board_grid).asInstanceOf[GridView]
    var vBoardGridAdapter: BoardAdapter = null

    override def onCreate(savedInstanceState: Bundle) = {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        // Restore any saved state.
        state = savedInstanceState match {
            case null => new State
            case _ => savedInstanceState.getParcelable[State](State.KEY) match {
                case state: State => state
                case _ => new State
            }
        }
        state.attachListener(this)
        controller = new Controller(state)

        vDifficultySeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener {
            override def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) = setDifficultyText(progress + 1)
            override def onStartTrackingTouch(seekBar: SeekBar) = {}
            override def onStopTrackingTouch(seekBar: SeekBar) = state.setDifficulty(vDifficultySeekBar.getProgress + 1)
        })

        val vBoardGridContainer = findViewById(R.id.board_grid_container)
        vBoardGridAdapter = new BoardAdapter(MainActivity.this, state, vBoardGridContainer, this)

        vBoardGrid.setNumColumns(Board.WIDTH)
        vBoardGrid.setAdapter(vBoardGridAdapter)
        vBoardGrid.setOnItemClickListener(new OnItemClickListener {
            override def onItemClick(parent: AdapterView[_], View: View, position: Int, id: Long): Unit = if (!state.board.aiThinking) {
                val col: Int = parent.getAdapter.asInstanceOf[BoardAdapter].column(position)
                state.board.add(col)
                state.board.checkWinner(col)
            }
        })
        setWidgetColour(R.color.pretty_white)
        state.callAllListenerFunctions()
    }

    /** Filter to allow changing colour of a View (specifically, the difficulty seek bar and progress indicator). */
    def makeColourFilter(colour: Int) = new PorterDuffColorFilter(getResources.getColor(colour), PorterDuff.Mode.SRC_IN)

    /** Sets the colour of the difficulty seek bar and progress indicator. */
    def setWidgetColour(colour: Int) = {
        val filter = makeColourFilter(colour)
        vDifficultySeekBar.getProgressDrawable.setColorFilter(filter)
        vDifficultySeekBar.getThumb.setColorFilter(filter)
        vThinkingIndicator.getIndeterminateDrawable.setColorFilter(filter)
    }

    override def onDestroy() = {
        super.onDestroy()
        state.removeListener(this)
        controller.onDestroy()
    }

    override def onSaveInstanceState(savedInstanceState: Bundle) = savedInstanceState.putParcelable(State.KEY, state)

    override def onCreateOptionsMenu(menu: Menu) = {
        getMenuInflater.inflate(R.menu.main, menu)
        vPlayerAAiToggle = menu.findItem(R.id.player_A_toggle)
        vPlayerBAiToggle = menu.findItem(R.id.player_B_toggle)
        state.playerAi.keys.foreach(piece => onPlayerAiChanged(piece, state.playerAi(piece)))
        getActionBar.setHomeButtonEnabled(false)
        true
    }

    override def onOptionsItemSelected(item: MenuItem): Boolean = {
        item.getItemId match {
            case R.id.new_game => state.newGame()
            case R.id.player_A_toggle => state.togglePlayerAi(YELLOW)
            case R.id.player_B_toggle => state.togglePlayerAi(RED)
        }
        true
    }

    override def onStartAiThinking() = {
        vDifficultySeekBar.setVisibility(View.INVISIBLE)
        vThinkingIndicator.setVisibility(View.VISIBLE)
    }

    override def onStopAiThinking() = {
        vDifficultySeekBar.setVisibility(View.VISIBLE)
        vThinkingIndicator.setVisibility(View.INVISIBLE)
    }

    override def onDifficultyChanged(difficulty: Int) = {
        vDifficultySeekBar.setProgress(difficulty - 1)
        setDifficultyText(difficulty)
    }

    def setDifficultyText(difficulty: Int) = vDifficultyText.setText("Difficulty: " + difficulty)

    override def onPlayerAiChanged(player: Player, isAi: Boolean) = player match {
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

    override def onGameEnd(winner: Winner) = {
        val message = getResources.getString(winner.win_text)
        Toast.makeText(getBaseContext, message, Toast.LENGTH_LONG).show()
    }
}
