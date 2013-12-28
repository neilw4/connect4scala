package uk.co.neilw4.c4scala

import android.app.Activity
import android.widget.LinearLayout
import android.widget.ImageView
import android.widget.SeekBar
import android.widget.TextView
import android.view.View
import android.app.Fragment
import android.os.Bundle
import android.view.MenuInflater
import android.view.Menu
import android.view.ViewGroup
import android.view.ViewGroup.LayoutParams
import android.view.LayoutInflater
import android.view.MenuItem
import android.content.Context
import android.widget.GridView
import android.widget.AdapterView.OnItemClickListener
import android.os.Bundle
import android.app.Fragment
import android.view.View.OnLayoutChangeListener
import android.widget.RelativeLayout

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
            override def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) = if (fromUser) state.difficulty = progress
            override def onStartTrackingTouch(seekBar: SeekBar) = {}
            override def onStopTrackingTouch(seekBar: SeekBar) = {}
        })
        vDifficultyText = findViewById(R.id.difficulty_text).asInstanceOf[TextView]

        vBoardGridContainer = findViewById(R.id.board_grid_container)
        vBoardGrid = findViewById(R.id.board_grid).asInstanceOf[GridView]
        vBoardGridAdapter = new BoardAdapter(this, state, vBoardGridContainer, this)
        vBoardGrid.setNumColumns(state.width)
        vBoardGrid.setAdapter(vBoardGridAdapter)
        state.callAllListeners
    }

    override def onDestroy = {
      super.onDestroy
      state.removeListener(this)
    }

    override def onSaveInstanceState(savedInstanceState: Bundle) = savedInstanceState.putParcelable(State.KEY , state)

    override def onCreateOptionsMenu(menu: Menu) = {
        getMenuInflater.inflate(R.menu.main, menu)
        vPlayerAAiToggle = menu.findItem(R.id.player_A_toggle)
        vPlayerBAiToggle = menu.findItem(R.id.player_B_toggle)
        Array.tabulate(state.playerAi.length)(i => onPlayerAiChanged(state.playerAi(i), i))
        true
    }

    override def onOptionsItemSelected(item: MenuItem): Boolean = {
        item.getItemId match {
            case R.id.new_game => state.newGame
            case R.id.player_A_toggle => state.setPlayerAi(!state.playerAi(0), 0)
            case R.id.player_B_toggle => state.setPlayerAi(!state.playerAi(1), 1)
        }
        true
    }

    override def onDifficultyChanged(difficulty: Int) = {
        vDifficultySeekBar.setProgress(difficulty)
        vDifficultyText.setText("Difficulty: " + difficulty + "%")
    }

    override def onPlayerAiChanged(playerAi: Boolean, i: Int) = i match {
        case 0 => if (vPlayerAAiToggle != null) vPlayerAAiToggle.setIcon(playerAi match {
            case true => R.drawable.yellow_ai
            case false => R.drawable.yellow_user
        })
        case 1 => if (vPlayerBAiToggle != null) vPlayerBAiToggle.setIcon(playerAi match {
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

    override def onBoardPieceChanged(piece: Piece, x: Int, y: Int) = vBoardGridAdapter.notifyDataSetChanged

}
