function [bw_stack, index_stack, composite_stack] = drawroi(image, num_roi, overlap, showfigs)

    arguments
       image
       num_roi {mustBeInteger, mustBePositive} = 1
       overlap {mustBeMember(overlap, [0, 1])} = 0
       showfigs {mustBeMember(showfigs, [0, 1])} = 0
    end
    
    blackroi = load('blackroi.mat');
    I = image; %imread(image);
    
    bw_stack = zeros(size(I, 1), size(I, 2), num_roi);
    index_stack = bw_stack;
    
    figure;
    imshow(I);
    hold on
    
    for i = 1:num_roi
        message = sprintf('Please select ROI shape');
        reply = questdlg(message, 'Polygon Selection', 'Circle', 'Rectangle', 'Assisted', 'Assisted');
        drawnow
        
        if reply == "Circle"
            roi = drawcircle('LineWidth', 1, 'MarkerSize', 6);
            addlistener(roi,'ROIClicked',@clickCallback);
            uiwait;
            bw_stack(:,:,i) = logical(createMask(roi,I));
        elseif reply == "Rectangle"
            roi = drawrectangle('LineWidth', 1, 'MarkerSize', 6);
            addlistener(roi,'ROIClicked',@clickCallback);
            uiwait;
            bw_stack(:,:,i) = logical(createMask(roi,I));
        elseif reply == "Assisted"
            roi = drawpolygon('LineWidth', 1, 'MarkerSize', 6);
            addlistener(roi,'ROIClicked',@clickCallback);
            uiwait;
            bw_stack(:,:,i) = logical(createMask(roi,I));
        else
            close gcf
            return;
        end      
        
        index_stack(:,:,i) = bw_stack(:,:,i) * i;
    end
        
    close gcf
    
    if overlap == 0
        composite_stack = zeros(size(I, 1), size(I, 2), num_roi);
        for i = 1:num_roi
            current_mask = index_stack(:,:,i);
            curr_sum = sum(composite_stack, 3);
            composite_stack(:, :, i) = current_mask .* (curr_sum == 0);
            index_stack(:,:,i) = composite_stack(:,:,i);
            bw_stack(:,:,i) = index_stack(:,:,i) > 0;
        end
        bw_stack = logical(bw_stack);
        composite_stack = sum(composite_stack, 3);
    else
        composite_stack = sum(bw_stack,3);
        composite_stack = composite_stack > 0;
    end
    
    if showfigs == 1
        set(figure, 'WindowStyle', 'Docked')
        imshow(I, 'InitialMagnification', 'fit', 'Border', 'tight')
        axis square
        hold on
        for i = 1:num_roi
            B = bwboundaries(bw_stack(:,:,i));
            for k = 1:length(B)
                boundary = B{k};
                plot(boundary(:,2), boundary(:,1), 'r', 'LineWidth', 2);
            end
        end
        
        set(figure, 'WindowStyle', 'Docked');
        imagesc(composite_stack);
        axis square
        colorbar
        colormap(blackroi.blackroi);
    end
end
    
function clickCallback(roi,evt)
    if strcmp(evt.SelectionType,'double')
        roi.InteractionsAllowed = 'none';
        roi.Color = [1 0 0];
        uiresume;
    end
end
