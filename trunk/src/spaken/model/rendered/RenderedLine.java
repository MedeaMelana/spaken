package spaken.model.rendered;

import java.awt.Graphics2D;
import java.awt.geom.Line2D;

import spaken.model.*;

public class RenderedLine implements Rendered {
  
  private static final double VERY_LARGE_NUMBER = Math.pow(10,6);
  
	private Pos p1;
	private Pos p2;

	public RenderedLine(Pos p1, Pos p2) {
		this.p1 = p1;
		this.p2 = p2;
	}

	public void draw(Graphics2D g, double pixelSize) {
	  Pos d = p2.subtract(p1);
	  try {
	    d = d.normalise().scale(VERY_LARGE_NUMBER);
	  } catch (NullVectorException e) {
	    d = Pos.ZERO;
	  }
	  Pos p1ext = p1.add(d);
	  Pos p2ext = p2.subtract(d);
	  
		g.draw(new Line2D.Double(p1ext.x, p1ext.y, p2ext.x, p2ext.y));
	}

}
