package spaken.model;

public class Intersections {
  
  public static Point intersect(final Line l1, final Line l2) {
    return new Point() {
      public double getCoor(boolean giveX) {
        Point p1 = l1.getP1();
        Point p2 = l1.getP2();
        Point p3 = l2.getP1();
        Point p4 = l2.getP2();
        
        double p1x = p1.getX();
        double p2x = p2.getX();
        double p3x = p3.getX();
        double p4x = p4.getX();
        double p1y = p1.getY();
        double p2y = p2.getY();
        double p3y = p3.getY();
        double p4y = p4.getY();
        
        // formule van http://en.wikipedia.org/wiki/Line-line_intersection
        
        double a, b;
        
        if (giveX) {
          a = p3x - p4x;
          b = p1x - p2x;
        } else {
          a = p3y - p4y;
          b = p1y - p2y;
        }
        
        return ((p1x*p2y - p1y*p2x) * a - b * (p3x*p4y - p3y*p4x))
             / ((p1x - p2x) * (p3y - p4y) - (p1y - p2y) * (p3x - p4x));
      }
      
    
      public double getX() {
        return getCoor(true);
      }
      
      public double getY() {
        return getCoor(false);
      }
    };
  }
  
  public static Point[] intersect(Circle c1, Circle c2) {
    return new Point[2];
  }
	
  public static Point[] intersect(Line l, Circle c) {
    return new Point[2];
  }
  
}
