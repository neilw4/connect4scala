package neilw4.c4scala.util

import android.content.Context
import android.graphics.{Canvas, Paint}
import android.util.AttributeSet
import android.view.View

/**
 * Created by neilw4 on 25/07/14.
 */
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
