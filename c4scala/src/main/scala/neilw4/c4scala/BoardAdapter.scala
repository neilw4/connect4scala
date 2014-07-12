package neilw4.c4scala

import android.widget.BaseAdapter
import android.content.Context
import android.widget.ImageView
import android.view.View
import android.view.ViewGroup
import android.widget.AbsListView

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
        var view: ImageView = convertView match {
            case tView: ImageView => tView
            case _ => new ImageView(context, null, 0)
        }
        view.setImageResource(getItem(position).resource)
        view.setScaleType(ImageView.ScaleType.FIT_CENTER)
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
