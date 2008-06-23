package spaken.model;

public class Intersections {
  
  public static Point intersect(final Line l1, final Line l2) {
    return new Point() {
      public Pos getPos() throws ImaginaryPointException {
        Pos p1 = l1.getP1().getPos();
        Pos p2 = l1.getP2().getPos();
        Pos p3 = l2.getP1().getPos();
        Pos p4 = l2.getP2().getPos();
        
        // formule van http://en.wikipedia.org/w/index.php?title=Line-line_intersection&oldid=210305729
        
        double w = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x);
        
        if (w == 0) {
          throw new ImaginaryPointException();
        }
        
        double u = p1.x * p2.y - p1.y * p2.x;
        double v = p3.x * p4.y - p3.y * p4.x;
        
        double ax = p3.x - p4.x;
        double bx = p1.x - p2.x;
        double ay = p3.y - p4.y;
        double by = p1.y - p2.y;
        
        return new Pos((u * ax - bx * v) / w, (u * ay - by * v) / w);
      }
    };
  }
  
  public static Point[] intersect(Circle c1, Circle c2) {
    return new Point[2];
  }
	
  public static Point[] intersect(Line l, Circle c) {
    return new LCIPoint[] {new LCIPoint(l, c, 1), new LCIPoint(l, c, -1)};
  }
  
  public static Point[] intersect(Circle c, Line l) {
    return intersect(l, c);
  }
  
  private static class LCIPoint implements Point {
    private final Line l;
    private final Circle c;
    private final double mul;
    
    private LCIPoint(Line l, Circle c, double mul) {
      assert mul == -1 || mul == 1;

      this.l = l;
      this.c = c;
      this.mul = mul;
    }
    
    public Pos getPos() throws ImaginaryPointException {
      Pos p1 = l.getP1().getPos();
      Pos p2 = l.getP1().getPos();
      Pos cc = c.getCenter().getPos();
      Pos cf = c.getDistFrom().getPos();
      Pos ct = c.getDistTo().getPos();
      
      // transform line relative to center of circle
      p1 = p1.subtract(cc);
      p2 = p2.subtract(cc);
      
      double r = cf.distance(ct);
      
      Pos d = p2.subtract(p1);
      double dr = d.size();
      double dm = p1.x * p2.y - p2.x * p1.y;
      
      double c = dr*dr;
      
      if (c == 0) {
        // eigenlijk een ongeldige lijn, ofzo, maar goed, dat levert ook geen punt op
        throw new ImaginaryPointException();
      }
      
      double a = dm * d.y;
      double b = Math.signum(d.y) * d.x * Math.sqrt(r*r * dr*dr - dm*dm);
      
      double x = (a + mul * b) / c;
      
      a = -dm * d.x;
      b = Math.abs(d.y) * Math.sqrt(r*r * dr*dr - dm*dm);
      
      double y = (a + mul * b) / c;
      
      return new Pos(x, y).add(cc);
    }
  }
  
}
