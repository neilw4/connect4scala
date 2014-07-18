package neilw4.c4scala

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.graphics.Canvas
import android.graphics.Paint
import android.util.AttributeSet
import android.widget.AbsListView
import android.widget.BaseAdapter
import android.widget.ImageView

trait BoardSizeSetter {
    def setBoardSize(width: Int, height: Int)
}

class BoardAdapter(context: Context, state: State, parent: View, boardSizeSetter: BoardSizeSetter) extends BaseAdapter {

    setSize()

    parent.addOnLayoutChangeListener(new View.OnLayoutChangeListener() {
        override def onLayoutChange(v: View, left: Int, top: Int, right: Int, bottom: Int, oldLeft: Int, oldTop: Int, oldRight: Int, oldBottom: Int) = {
            if (left - right != oldLeft - oldRight || top - bottom != oldTop - oldBottom) {
                setSize()
            }
        }

    })

    var size = 0

    def setSize() = {
            size = if (parent.getHeight / Board.HEIGHT > parent.getWidth / Board.WIDTH) parent.getWidth / Board.WIDTH else parent.getHeight / Board.HEIGHT
            boardSizeSetter.setBoardSize(size * Board.WIDTH, size * Board.HEIGHT)
            notifyDataSetChanged()
    }

    override val getCount = Board.WIDTH * Board.HEIGHT

    override def getItem(position: Int) = state.board(column(position))(row(position))

    override def getItemId(position: Int) = position

    override def getView(position: Int, convertView: View, group: ViewGroup): View = {
        val view: CircleView = convertView match {
            case tView: CircleView => tView
            case _ => new CircleView(context, 0.2)
        }
        view.setColour(getItem(position).colour)
        var layout = view.getLayoutParams
        if (layout == null) {
            layout = new AbsListView.LayoutParams(size, size)
        } else {
            layout.height = size
            layout.width = size
        }
        view.setLayoutParams(layout)
        view
    }

    def column(position: Int) = position % Board.WIDTH
    def row(position: Int) = Board.HEIGHT - position / Board.WIDTH - 1
}

class CircleView(context: Context, paddingPc: Double) extends View(context) {

    def this(context: Context, attrs: AttributeSet) = this(context, 0)

    var fill: Paint = null

    override def onDraw(canvas: Canvas) = {
        super.onDraw(canvas)
        val cx = getWidth() / 2
        val cy = getHeight() / 2
        val radius = math.min(cx, cy) * (1 - paddingPc)
        canvas.drawCircle(cx, cy, radius.asInstanceOf[Int], fill)
    }

    def setColour(rColour: Int) = {
        fill = new Paint()
        fill.setAntiAlias(true)
        fill.setStyle(Paint.Style.FILL)
        fill.setColor(getResources.getColor(rColour))
    }
}
