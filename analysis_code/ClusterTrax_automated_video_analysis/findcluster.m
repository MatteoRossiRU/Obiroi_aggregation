function [BWimg, ROIcx, ROIcy, Density_Capitalization, Tile, ROIzoom, MMCimg, GSimg] =...
    findcluster(OGimg, threshold, background, showfigs, tilex, stepx, tiley, stepy)

    arguments
        OGimg
        threshold {mustBeInRange(threshold, 0, 1)} = .2
        background {mustBeMember(background,[0, 1])} = 1
        showfigs {mustBeMember(showfigs,[0, 1])} = 1
        tilex {mustBePositive} = round(.05 * size(OGimg, 1) * size(OGimg, 2))
        stepx {mustBeLessThanOrEqual(stepx, tilex)} = round(.25 * tilex)
        tiley {mustBePositive} = tilex
        stepy {mustBeLessThanOrEqual(stepy, tiley)} = stepx
    end
    
    % Should include Dispersion index, i.e. variance
    % Should include num blobs outside cluster tile
    
    if tilex > size(OGimg, 2) || tiley > size(OGimg, 1)
        disp("Your tile cannot be larger than the size of your image!");
        return
    end
        
    GSimg = im2gray(im2double(OGimg));
    
    if background == 1
        BWimg = GSimg < threshold;
    else
        BWimg = GSimg > threshold;
    end
    
    BWimg = imfill(BWimg,'holes');
    MMCimg = BWimg;
    
    imagey = size(BWimg, 1);
    imagex = size(BWimg, 2);
%     ROIzoom = zeros(tilex, tiley);
    xshift = idivide(int16(imagex - tilex), stepx);
    yshift = idivide(int16(imagey - tiley), stepy);
    xstop = (stepx * xshift) + 1;
    ystop = (stepy * yshift) + 1;
    ROIzoom = BWimg(1:1 + tiley - 1, 1:1 + tilex - 1);
    roix1 = 1;
    roix2 = 1 + tilex - 1;
    roiy1 = 1;
    roiy2 = 1 + tiley - 1;
    
    % Tile from top-left
    for m = 1 : stepy : ystop
        for n = 1 : stepx : xstop
            if sum(BWimg(m:m + tiley - 1, n:n + tilex - 1), 'all') > sum(ROIzoom, 'all')
                ROIzoom = BWimg(m:m + tiley - 1, n:n + tilex - 1);
                roix1 = n;
                roix2 = n + tilex - 1;
                roiy1 = m;
                roiy2 = m + tiley - 1;
            end
        end
    end
    
    % Tile from bottom-left
%     for m = imagey : -stepy : imagey - (yshift * stepy)
%         for n = 1 : stepx : xstop
%             if sum(BWimg(m:m - tiley + 1, n:n + tilex - 1), 'all') > sum(ROIzoom, 'all')
%                 ROIzoom = BWimg(m:m - tiley + 1, n:n + tilex - 1);
%                 roix1 = n;
%                 roix2 = n + tilex - 1;
%                 roiy1 = m - tiley + 1;
%                 roiy2 = m;
%             end
%         end
%     end
    
    for m = imagey
        for n = 1 : stepx : xstop
            if sum(BWimg(m:m - tiley + 1, n:n + tilex - 1), 'all') > sum(ROIzoom, 'all')
                ROIzoom = BWimg(m:m - tiley + 1, n:n + tilex - 1);
                roix1 = n;
                roix2 = n + tilex - 1;
                roiy1 = m - tiley + 1;
                roiy2 = m;
            end
        end
    end
    
    % Tile from top-right
%     for m = 1 : stepy : ystop
%         for n = imagex : -stepx : imagex - (xshift * stepx)
%             if sum(BWimg(m:m + tiley - 1, n:n - tilex + 1), 'all') > sum(ROIzoom, 'all')
%                 ROIzoom = BWimg(m:m + tiley - 1, n:n - tilex + 1);
%                 roix1 = n - tilex + 1;
%                 roix2 = n;
%                 roiy1 = m;
%                 roiy2 = m + tiley - 1;
%             end
%         end
%     end
    
    for m = 1 : stepy : ystop
        for n = imagex
            if sum(BWimg(m:m + tiley - 1, n:n - tilex + 1), 'all') > sum(ROIzoom, 'all')
                ROIzoom = BWimg(m:m + tiley - 1, n:n - tilex + 1);
                roix1 = n - tilex + 1;
                roix2 = n;
                roiy1 = m;
                roiy2 = m + tiley - 1;
            end
        end
    end
    
    % Tile from bottom-right
%     for m = imagey : -stepy : imagey - (yshift * stepy)
%         for n = imagex : -stepx : imagex - (xshift * stepx)
%             if sum(BWimg(m:m - tiley + 1, n:n - tilex + 1), 'all') > sum(ROIzoom, 'all')
%                 ROIzoom = BWimg(m:m - tiley + 1, n:n - tilex + 1);
%                 roix1 = n - tilex + 1;
%                 roix2 = n;
%                 roiy1 = m - tiley + 1;
%                 roiy2 = m;
%             end
%         end
%     end
    
    for m = imagey
        for n = imagex : -stepx : imagex - (xshift * stepx)
            if sum(BWimg(m:m - tiley + 1, n:n - tilex + 1), 'all') > sum(ROIzoom, 'all')
                ROIzoom = BWimg(m:m - tiley + 1, n:n - tilex + 1);
                roix1 = n - tilex + 1;
                roix2 = n;
                roiy1 = m - tiley + 1;
                roiy2 = m;
            end
        end
    end
    
    for m = imagey : -stepy : imagey - (yshift * stepy)
        for n = imagex
            if sum(BWimg(m:m - tiley + 1, n:n - tilex + 1), 'all') > sum(ROIzoom, 'all')
                ROIzoom = BWimg(m:m - tiley + 1, n:n - tilex + 1);
                roix1 = n - tilex + 1;
                roix2 = n;
                roiy1 = m - tiley + 1;
                roiy2 = m;
            end
        end
    end
    
    Tile = polyshape([roix1 roix1 roix2 roix2],[roiy1 roiy2 roiy2 roiy1]);
    h = poly2mask(Tile.Vertices(:,1), Tile.Vertices(:,2), size(BWimg,2), size(BWimg,1));
    MMCimg(~h) = 0;
    [ROIcx, ROIcy] = centroid(Tile);
    Density_Capitalization = sum(ROIzoom, 'all')/sum(BWimg, 'all');
    
    if showfigs == 1
        set(figure, 'WindowStyle', 'Docked')
        
        subplot(2, 2, 1)
        imshow(OGimg, 'InitialMagnification', 'fit', 'Border', 'tight');
        
        subplot(2, 2, 2)
        imshow(GSimg, 'InitialMagnification', 'fit', 'Border', 'tight');
        
        subplot(2, 2, 3)
        imshow(BWimg, 'InitialMagnification', 'fit', 'Border', 'tight');
        hold on
        plot(ROIcx, ROIcy, 'r+', 'MarkerSize', 10, 'LineWidth', 1);
        plot(Tile);
        
        subplot(2, 2, 4)
        imshow(ROIzoom, 'InitialMagnification', 'fit', 'Border', 'tight');
    end
    
end